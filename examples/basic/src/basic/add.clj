(ns basic.add
  (:require [com.phronemophobic.clj-webgpu.compute :as gpu]))

(def shader-src
  "

// Declare the storage buffers
@group(0) @binding(0) var<storage, read> v1: array<f32>;
@group(0) @binding(1) var<storage, read> v2: array<f32>;
@group(0) @binding(2) var<storage, read_write> out : array<f32>;


// Entry point for the compute shader
@compute
@workgroup_size(1)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
  var i = global_id.x;
  if (i < arrayLength(&out)) {
    out[i] = v1[i]+ v2[i];
  }

}
")

(defn add-cpu [v1 v2]
  (float-array
   (eduction
    (map (fn [i]
           (+ (aget v1 i)
              (aget v2 i))))
    (range (alength v1)))))

(defn add-gpu [v1 v2]
  (let [len (alength v1)

        ctx (gpu/create-context)

        buf1 (gpu/create-buffer ctx {:usage #{:Storage :CopyDst :CopySrc}
                                     :type :f32
                                     :length (alength v1)})
        buf2 (gpu/create-buffer ctx {:usage #{:Storage :CopyDst :CopySrc}
                                     :type :f32
                                     :length (alength v2)})

        out (gpu/create-buffer ctx {:usage #{:Storage :CopySrc}
                                    :type :f32
                                    :length (alength v2)})

        staging-buf (gpu/create-buffer ctx {:usage #{:MapRead :CopyDst}
                                            :type :f32
                                            :length (alength v1)})

        shader (gpu/create-shader ctx {:src shader-src})


        _ (gpu/copy-to-buffer ctx buf1 v1)
        _ (gpu/copy-to-buffer ctx buf2 v2)

        _ (gpu/compute ctx {:shader shader
                             :workgroups {:x (alength v1)}
                             :bindings [buf1 buf2 out]})

        _ (gpu/copy ctx out staging-buf)
        result (gpu/copy-from-buffer ctx staging-buf)]
    result))

(defn -main [& args]
  

  (def len 65535)
  (def v1 (float-array (repeatedly len rand)))
  (def v2 (float-array (repeatedly len rand)))

  

  (time (add-gpu v1 v2))
  (time (add-cpu v1 v2))
  
  )
