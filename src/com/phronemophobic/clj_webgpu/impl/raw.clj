(ns com.phronemophobic.clj-webgpu.impl.raw
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [com.rpl.specter :as specter]
            [ham-fisted.api :as hamf]
            membrane.skia
            [tech.v3.datatype.native-buffer :as native-buffer]
            [com.phronemophobic.clong.gen.dtype-next :as gen]
            [com.phronemophobic.clong.gen.jna.insn :as gen.jna.insn]
            [com.phronemophobic.clong.gen.jna.util :as util]
            [insn.core :as insn]
            [tech.v3.datatype.ffi.jna :as dtype.ffi.jna]
            [tech.v3.datatype.ffi.base :as dtype.ffi.base]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype :as dtype]
            #_[com.phronemophobic.clong.gen.jna :as gen])
  (:import
   com.sun.jna.Callback
   java.io.PushbackReader
   java.io.File
   java.util.Map
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.LongByReference
   com.sun.jna.Structure
   java.lang.ref.Cleaner)
  (:gen-class))

;; v29.0.1.1
;; https://github.com/gfx-rs/wgpu-native/releases/tag/v29.0.1.1
(def distro-dir
  (io/file "/Users/adrian/Downloads/wgpu-macos-aarch64-debug/")
  )

(defonce ^:private cleaner
  (delay (Cleaner/create)))

(def libwebgpu
  (com.sun.jna.NativeLibrary/getInstance 
   (File/.getCanonicalPath (io/file distro-dir "lib/libwgpu_native.dylib")))
  #_(com.sun.jna.NativeLibrary/getInstance "wgpu_native"))

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
  
  (def new-api 
    (clang/easy-api (.getCanonicalPath (io/file
                                        "/Users/adrian/Downloads/wgpu-macos-aarch64-debug/include/webgpu/wgpu.h"))))
  
  
  (def new-children
    (-> (clang/parse (.getCanonicalPath (io/file
                                         "/Users/adrian/Downloads/wgpu-macos-aarch64-debug/include/webgpu/wgpu.h"))
                     clang/default-arguments)
        (clang/get-children)
        (->> (into []
                   clang/default-api-xforms))))
  

  (require '[com.phronemophobic.clong.clang.jna.raw :as c])
  

  ,)

(defn ^:private clang-resolve [sym]
  @(requiring-resolve (symbol "com.phronemophobic.clong.clang.jna.raw"
                              (name sym))))
  
(defn ^:private all-typedef-enum-kinds [nodes]
  (let [clang_getTypedefDeclUnderlyingType (clang-resolve 'clang_getTypedefDeclUnderlyingType)
        clang_getTypeSpelling (clang-resolve 'clang_getTypeSpelling)
        clang_getCString (clang-resolve 'clang_getCString)]
    (->> nodes
         (filter (fn [node]
                   (= "CXType_Typedef" (:type node))))
         (filter (fn [node]
                   (let [underlying-type (-> node
                                             :cur
                                             clang_getTypedefDeclUnderlyingType
                                             clang_getTypeSpelling
                                             clang_getCString)]
                     (= "WGPUFlags" underlying-type))))
         (map :spelling))))

(defn ^:private extract-flags [nodes]
    (let [
          clang_getCursorType (clang-resolve 'clang_getCursorType)
          clang_getTypeSpelling (clang-resolve 'clang_getTypeSpelling)
          clang_getCString (clang-resolve 'clang_getCString)
          clang_Cursor_getVarDeclInitializer (clang-resolve 'clang_Cursor_getVarDeclInitializer )

          clang_Cursor_Evaluate (clang-resolve 'clang_Cursor_Evaluate)
          clang_EvalResult_getAsLongLong (clang-resolve 'clang_EvalResult_getAsLongLong )
          clang_EvalResult_dispose (clang-resolve 'clang_EvalResult_dispose)
          
          clang_Cursor_getRawCommentText (clang-resolve 'clang_Cursor_getRawCommentText)

          get-snippet (requiring-resolve 'com.phronemophobic.clong.clang/get-snippet)

          enum-kinds (into {}
                           (map (fn [kind]
                                  [(str "const " kind)
                                   kind]))
                           (all-typedef-enum-kinds nodes))]
      (->> nodes
           (filter (fn [node]
                     (= (:kind node)
                        "CXCursor_VarDecl")))
           (keep (fn [node]
                     (let [type (-> node
                                    :cur
                                    clang_getCursorType
                                    clang_getTypeSpelling
                                    clang_getCString)]
                       (when-let [enum-kind (get enum-kinds type)]
                         
                         (let [eval-result (clang_Cursor_Evaluate (:cur node))
                               _ (when (nil? eval-result)
                                   (throw (ex-info "Could not evaluate expression"
                                                   {:node node})))
                               value (clang_EvalResult_getAsLongLong eval-result)
                               _ (clang_EvalResult_dispose eval-result)]
                           {:name (:spelling node)
                            :flag enum-kind
                            
                            :raw-comment (-> node
                                             :cur
                                             clang_Cursor_getRawCommentText
                                             clang_getCString)
                            :value value}))))))))


(defn parse-api []
  (let [path (.getCanonicalPath (io/file distro-dir
                                         "include"
                                         "webgpu"
                                         "wgpu.h"))
        api ((requiring-resolve 'com.phronemophobic.clong.clang/easy-api)
             path)
        nodes (->> ((requiring-resolve 'com.phronemophobic.clong.clang/parse)
                    path
                    @(requiring-resolve 'com.phronemophobic.clong.clang/default-arguments))
                   ((requiring-resolve 'com.phronemophobic.clong.clang/get-children))
                   (into []
                         @(requiring-resolve 'com.phronemophobic.clong.clang/default-api-xforms)))
        flags (extract-flags nodes)]
    (assoc api :flags flags)))


(comment
  (parse-api)
  (extract-flags new-children)
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
                 (parse-api)))))

(comment
  (dump-api {})
  ,)

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

#_(defn fix-char-pointers [api]
  (specter/setval
   [gen/ALL-TYPES
    #(= % [:coffi.mem/pointer :coffi.mem/char])]
   :coffi.mem/pointer
   api))

#_(defn fixes [api]
  (-> api
      (set-field-type :clong/WGPUShaderModuleDescriptor "nextInChain" :coffi.mem/pointer)
      fix-char-pointers))

(def api (-> raw-api
             ;; fixes
             ;; gen/replace-forward-references
             ))

;; (gen/def-api libwebgpu api)
(def dtype-api (gen/api->library-interface api))
(def dtype-structs (gen/api->structs api))
(doseq [[id fields] dtype-structs]
  (dt-struct/define-datatype! id fields))

(dt-ffi/define-library-interface dtype-api)

(first dtype-api)



;; [buf width height row-bytes])
;; (skia_direct_bgra8888_buffer buf (int width) (int height) (int row-bytes)))

;; make sure lib is loaded
(prn @@#'membrane.skia/membraneskialib)
(dt-ffi/define-library-interface
 {
  :skia_direct_buffer
   {:rettype :pointer
    :argtypes '[[buf :pointer]
                [colorType :int32]
                [width :int32]
                [height :int32]
                [row-bytes :int32]],}})

(defmacro def-flags []
  `(do
     ~@(eduction
        (map (fn [{:keys [name flag raw-comment value]}]
               `(def  ~(symbol name)
                       ~@(when-let [doc raw-comment]
                           (when (not= doc "")
                             [doc]))
                  ~value)))
        (:flags api))))

(def-flags)

(gen/def-enums api)


;; (gen/def-struct-constructors (:structs api))


#_
(def ^:private
  array-info-by-id
  (into {}
        (map (fn [{:keys [id size-in-bytes]}]
               [id {:size-in-bytes size-in-bytes
                    :class (Class/forName (str (gen/ns-struct-prefix *ns*) "." (name id) "ByReference"))
                    :mergef @(ns-resolve *ns* (symbol (str "merge->" (name id))))}]))
        (:structs api)))

#_
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

(defn struct-array [dtype ms]
  (let [arr (dt-struct/new-array-of-structs 
             dtype
             (count ms) 
             {:container-type :native-heap
              :resource-type :gc})]
    (doseq [[struct m] (map vector arr ms)
            [k v] m]
      (Map/.put struct k v))
    arr))

(defn ^:private struct-map-constructor [struct]
  (let [id (:id struct)
        dtype (-> id name keyword)
        fn-name (symbol (str "map->" (name id)))
        
        m {:keys (into []
                       (comp (map :name)
                             (map symbol))
                       (:fields struct))
           :as 'm}
        
        doc 
        (str/join
         "\n"
         (eduction
          (map (fn [{:keys [name datatype type]}]
                 (str/join ", "
                           [name 
                            type
                            datatype])))
          (:fields struct)))]
    `(defn ~fn-name 
       ~doc
       ([]
        (~fn-name {}))
       ([~m]
        (dt-struct/map->struct ~dtype ~'m)))))


(defmacro ^:private all-struct-constructors []
  `(do
     ~@(eduction
        (map struct-map-constructor)
        (:structs api))))

(all-struct-constructors)


(def classname-base "com.phronemophobic.clj_webgpu.impl.raw")

;; (def defined-structs (hamf/java-linked-hashmap))
;; (doseq [class-def (-> defined-structs
;;                       .values)]
;;   (insn/define class-def))

(defn make-struct-class* [dtype]
  (let [defined-structs (hamf/java-linked-hashmap)]
    (dtype.ffi.jna/define-by-value-struct!
     classname-base
     defined-structs
     dtype)
    (insn/define (-> defined-structs .values first))))

(def make-struct-class (memoize make-struct-class*))

#_(defn dtype->by-value-struct-classname [dtype]
  (str classname-base "$" (munge (name dtype))))

(defn dtype->insn [dtype]
  (if (sequential? dtype)
    (make-struct-class (second dtype))
    (dtype.ffi.base/argtype->insn com.sun.jna.Pointer
                                  :ptr-as-platform
                                  dtype)))

(defn make-callback-interface* [ret-type arg-types]
  {:flags #{:public :interface}
   :interfaces [Callback]
   :name (symbol (str classname-base "." (util/callback-name classname-base ret-type arg-types)))
   :methods [{:flags #{:public :abstract}
              :name "callback"
              :desc
              (conj (mapv dtype->insn arg-types)
                    (dtype->insn ret-type))}]})

(defn make-callback-interface [ret-type arg-types]
  (let [class-def (make-callback-interface* ret-type arg-types)]
    (when *compile-files*
      (let [info (insn/visit class-def)]
        (insn/write info)))
    (insn/define class-def)))

(defonce ^:no-doc not-garbage
  (atom []))

(defn ^:no-doc preserve!
  "Store this value so it's not garbage collected"
  [x]
  (swap! not-garbage conj x)
  x)

(def make-callback-interface-memo (memoize make-callback-interface))

(defn callback-maker* [ret-type arg-types]
  (let [interface (make-callback-interface-memo ret-type arg-types)
        args (map (fn [i] (symbol (str "x" i)))
                  (range (count arg-types)))]
    `(fn [f#]
       (preserve! f#)
       (preserve!
        (reify
          ~(symbol (.getName ^Class interface))
          (~'callback [this# ~@args]
           (f# ~@args)))))))


(defmacro callback-maker [ret-type arg-types f]
  `(let [maker# ~(callback-maker* ret-type arg-types)
         callback# (maker# ~f)]
     (preserve! callback#)
     (com.sun.jna.Pointer/nativeValue
      (com.sun.jna.CallbackReference/getFunctionPointer callback#))))



(defn ->wgpu-string-view [s]
  (let [data (dt-ffi/string->c s)]
    (tech.v3.resource/track
     (map->WGPUStringView
      {:data (.address (dt-ffi/->pointer data))
       :length (dec (native-buffer/native-buffer-byte-len data))})
     {:dispose-fn (constantly data)})))

(defn wgpu-string-view->string [w]
  (let [len (:length w)]
    (if (pos? len)
      (let [bs (byte-array len)]
        (dtype/copy! (native-buffer/wrap-address (:data w)
                                                 len)
                     bs)
        (String. bs "utf-8"))
      "")))

(defmacro with-refs [& body]
  `(let [~'refs (volatile! [])
         ~'ref! (fn [o#]
                  (vswap! ~'refs conj o#)
                  o#)
         ~'->address (fn [o#]
                       (-> o#
                           ~'ref!
                           dt-ffi/->pointer
                           dt-ffi/pointer->address))]
     ~@body))