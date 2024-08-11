(ns com.phronemophobic.clj-webgpu.impl.raw
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [com.rpl.specter :as specter]
            [com.phronemophobic.clong.gen.jna :as gen])
  (:import
   java.io.PushbackReader
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.LongByReference
   com.sun.jna.Structure
   java.lang.ref.Cleaner)
  (:gen-class))

(defonce ^:private cleaner
  (delay (Cleaner/create)))

(def libwebgpu
  (com.sun.jna.NativeLibrary/getInstance "wgpu_native"))

(defn ^:private write-edn [w obj]
  (binding [*print-length* nil
            *print-level* nil
            *print-dup* false
            *print-meta* false
            *print-readably* true

            ;; namespaced maps not part of edn spec
            *print-namespace-maps* false

            *out* w]
    (pr obj)))

(comment
  (require '[com.phronemophobic.clong.clang :as clang])

  (def api
    (clang/easy-api (.getCanonicalPath (io/file "headers" "wgpu.h"))))
  

  ,)

(defn dump-api [opts]
  (let [outf (io/file
              "resources"
              "com"
              "phronemophobic"
              "webgpu_native"
              "api.edn")]
    (.mkdirs (.getParentFile outf))
    (with-open [w (io/writer outf)]
      (write-edn w
                 ((requiring-resolve 'com.phronemophobic.clong.clang/easy-api)
                  (.getCanonicalPath (io/file "headers"
                                              "wgpu.h")))))))

(defn load-api []
  (with-open [rdr (io/reader
                   (io/resource
                    "com/phronemophobic/webgpu_native/api.edn"))
              rdr (java.io.PushbackReader. rdr)]
    (edn/read rdr)))


(def raw-api (load-api))

(defn struct-by-id [id]
  [:structs
   specter/ALL
   #(= (:id %) id)])

(defn struct-field [id field]
  [(struct-by-id id)
   :fields
   specter/ALL
   #(= (:name %) field)])

(defn set-field-type [api id field type]
  (specter/transform
        (struct-field id field)
        (fn [field]
          (assoc field :datatype type))
        api))

(defn fix-char-pointers [api]
  (specter/setval
   [gen/ALL-TYPES
    #(= % [:coffi.mem/pointer :coffi.mem/char])]
   :coffi.mem/pointer
   api))

(defn fixes [api]
  (-> api
      (set-field-type :clong/WGPUShaderModuleDescriptor "nextInChain" :coffi.mem/pointer)
      fix-char-pointers))

(def api (-> raw-api
             fixes
             gen/replace-forward-references))

(gen/def-api libwebgpu api)


(gen/def-struct-constructors (:structs api))

(def ^:private
  array-info-by-id
  (into {}
        (map (fn [{:keys [id size-in-bytes]}]
               [id {:size-in-bytes size-in-bytes
                    :class (Class/forName (str (gen/ns-struct-prefix *ns*) "." (name id) "ByReference"))
                    :mergef @(ns-resolve *ns* (symbol (str "merge->" (name id))))}]))
        (:structs api)))

(defn struct-array [id ms]
  (when (seq ms)
    (let [{:keys [size-in-bytes mergef class] :as m} (get array-info-by-id id)
          _ (when (not m)
              (throw (ex-info "Unknown struct id"
                              {:id id
                               :ms ms})))
          mem (Memory. (* size-in-bytes (count ms)))

          first-struct (Structure/newInstance class
                                              (.share mem 0 size-in-bytes))
          ref (volatile! mem)]
      (.register ^Cleaner @cleaner first-struct
               (fn []
                 ;; hang onto memory
                 (vreset! ref nil)))
      (doseq [[i m] (map-indexed vector ms)]
        (let [struct (Structure/newInstance class
                                            (.share mem (* i size-in-bytes) size-in-bytes))]
          (mergef struct m)))
      (doto first-struct
        ;; otherwise, struct uses wrong data :(
        .read))))
