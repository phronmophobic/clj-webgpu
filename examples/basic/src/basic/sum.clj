(ns basic.sum
  (:require [com.phronemophobic.clj-webgpu.compute :as gpu]
            [com.phronemophobic.clj-webgpu.impl.raw :as raw])
  (:import com.sun.jna.Pointer))

(defn sum-cpu [v1]
  (reduce + (float 0.0) v1))

(def shader-src
  "
 
    @group(0) @binding(0) var<storage, read_write> sum: array<f32>;
    @group(0) @binding(1) var<uniform> stride: u32;
 
    @compute @workgroup_size(64, 1, 1) 
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {

      let i = global_id.x*stride*2u;
      if (i < arrayLength(&sum)) {
          sum[i] = sum[i] + sum[i+stride];
    }
}
")

(defn ceil [n]
  (Math/round
   (Math/ceil n)))

(defn sum-gpu [^floats v1]
  (let [len (alength v1)

        ctx (gpu/create-context)
        shader (gpu/create-shader ctx {:src shader-src})


        sum (gpu/create-buffer ctx {:usage #{:Storage :CopyDst :CopySrc}
                                    :type :f32
                                    :length len})

        _ (gpu/copy-to-buffer ctx sum v1)

        ;;const numSteps = Math.ceil(Math.log2(numChunks));
        num-steps (ceil (/ (Math/log len)
                           (Math/log 2)))
        uniforms (loop [stride 1
                        num-steps num-steps
                        uniforms []]
                   (if (pos? num-steps)
                     (let [uni (gpu/create-buffer ctx {:usage #{:Uniform}
                                                       :type :u32
                                                       :mapped-at-creation? true
                                                       :length 1})
                           ^Pointer buf (raw/wgpuBufferGetMappedRange (:buffer uni) 0 (:size uni))]
                       (.setInt buf 0 (int stride))
                       (raw/wgpuBufferUnmap (:buffer uni))
                       (recur (* 2 stride)
                              (dec num-steps)
                              (conj uniforms uni)))
                     ;; else
                     uniforms))

        staging-buf (gpu/create-buffer ctx {:usage #{:MapRead :CopyDst}
                                            :type :f32
                                            :length len})





        _ (doseq [i (range num-steps)
                  :let [stride (apply * (repeat i 2))
                        workgroup-x (ceil (/ len 64 stride))]]
            (gpu/dispatch ctx {:shader shader
                               :workgroups {:x workgroup-x}
                               :bindings [sum (nth uniforms i)]}))

        _ (gpu/copy ctx sum staging-buf)
        result (gpu/copy-from-buffer ctx staging-buf)]
    result))

(defn -main [& args]
  
  (def len 3709333)
  (def v1 (float-array (repeatedly len rand)))

  (prn (first (time (sum-gpu v1))))
  (prn (time (sum-cpu v1))))
