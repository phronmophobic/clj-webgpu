(ns com.phronemophobic.clj-webgpu.c2
  (:require [clojure.math :as math]
            [clojure.java.io :as io]
            
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            
            [com.phronemophobic.clj-webgpu.impl.raw :as raw]
            [com.phronemophobic.clj-webgpu.compute :as wgpu])
  
  (:gen-class))

(def shader-source
  "
struct Data {
  // Number of valid elements in `values`
  length : u32,
  // Array data (must be 4-byte aligned; f32 is fine)
  values : array<i32>,
};

@group(0) @binding(0)
var<storage, read_write> data : Data;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
  let i = gid.x;
  if (i >= data.length) {
    return;
  }
  data.values[i] = data.values[i] * 9;
}  
  ")

(defn example []
  (raw/with-refs
   (let [
         
         
         ctx (wgpu/create-context)
         
         shader (wgpu/create-shader ctx {:src shader-source})
         pipeline
         (wgpu/create-compute-pipeline  ctx
                                        {:shader shader})
         
         
         storage (wgpu/create-buffer ctx {:usage #{:Storage
                                                   :CopyDst
                                                   :CopySrc}
                                          :type :i32
                                          :length 17})
         
         _ (wgpu/copy-to-buffer ctx
                                storage
                                (dtype/make-container
                                 :native-heap
                                 :int32
                                 (into [17]
                                       (range 16))))
         
         staging (wgpu/create-buffer ctx {:usage #{:MapRead
                                                   :CopyDst}
                                          :type :i32
                                          :length 17})
         
         _ (do (prn "submitting...")
               (flush))
         
         _ (wgpu/submit
            ctx
            [
             
             {:compute-pass true
              :pipeline pipeline
              :bind-groups [[storage]]}
             {:copy-pass true
              :source storage
              :destination staging}
             ])
         _ (do (prn "we submitted!")
               (flush)) 
         
         ;; wait for queue to 
         _ (wgpu/poll (:device ctx) true)
         _ (do (prn "now save the texture...")
               (flush))
         
         result (wgpu/copy-from-buffer ctx staging)]
     (vreset! refs [])
     result)))


(defn -main [& args]
  (prn
   (example)))