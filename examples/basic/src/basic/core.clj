(ns basic.core
  (:require [com.phronemophobic.clj-webgpu.compute :as gpu]))

(def shader-src
  "
@group(0) @binding(0) var<storage, read_write> buf: array<i32>;

@compute
@workgroup_size(1)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
  var i = global_id.x;
  if (i < arrayLength(&buf)) {
    buf[i] = buf[i]+ 42;
  }
}

")

(defn -main [& args]
  (def ctx (gpu/create-context))

  (def nums (int-array (range 10)))


  (def buf (gpu/create-buffer ctx {:usage #{:Storage :CopyDst :CopySrc}
                                   :type :i32
                                   :length (count nums)}))

  (def staging-buf (gpu/create-buffer ctx {:usage #{:MapRead :CopyDst}
                                           :type :i32
                                           :length (count nums)}))

  (def shader (gpu/create-shader ctx {:src shader-src}))

  (gpu/copy-to-buffer ctx buf nums)

  (gpu/compute ctx {:shader shader
                    :workgroups {:x (alength nums)}
                    :bindings [buf]})

  (gpu/copy ctx buf staging-buf)
  (def result (gpu/copy-from-buffer ctx staging-buf))

  (prn (seq result))
  )
