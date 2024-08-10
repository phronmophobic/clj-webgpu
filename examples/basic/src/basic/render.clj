(ns basic.render
  (:require [com.phronemophobic.clj-webgpu.compute :as gpu]
            [clojure.java.io :as io]
            [clojure.core.matrix :as mat]

            [membrane.java2d :as java2d]
            [membrane.ui :as ui]
            [com.phronemophobic.clogif :as gif]

            [clj-manifold3d.core
              :refer [cube cylinder sphere cylinder extrude cross-section frame square circle
                      union difference intersection translate get-mesh export-mesh status rotate
                      hull revolve offset refine smooth loft scale material text scale-to-height
                      three-point-arc frame-2d transform slice tetrahedron slices polyhedron]]))


(def shader-src (slurp (io/file "shaders/render.wgsl")))

(def shader-src2
  "

      struct Uniforms {
        matrix: mat4x4f,
      };

@group(0) @binding(0) var<uniform> uni: Uniforms;
@group(0) @binding(1) var<storage, read> v1: array<f32>;
@group(0) @binding(2) var<storage, read_write> out : array<f32>;


@compute
@workgroup_size(1)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
  var i = global_id.x*3u;
var outi = global_id.x*4u;
  if (i < arrayLength(&out)) {
 var p = (uni.matrix* vec4f ( v1[i], v1[i+1], v1[i+2], 1.0));
    out[outi] = p.x;
    out[outi+1] = p.y;
    out[outi+2] = p.z;
    out[outi+3] = p.w;

  }
}

")


(defn mesh->verts [mesh]
  (let [pos (.vertPos mesh)
        buf (float-array
             (eduction
              (map (fn [idx]
                     (eduction
                      (map (fn [v]
                             [(.x v)
                              (.y v)
                              (.z v)]))
                      [(.get pos (.x idx))
                       (.get pos (.y idx))
                       (.get pos (.z idx))])))

              cat
              cat
              (.triVerts mesh)))]
    (identity mesh)
    buf))

;; shaders matrices are column major
;; this matrix is already column major
(defn perspective-fov 
  ([fovy aspect-ratio near-plane far-plane]
   (let [f (/ 1.0 (Math/tan (/ fovy 2.0)))
         nf (/ 1.0 (- near-plane far-plane))]
     [[(/ f aspect-ratio) 0 0 0]
      [0 f 0 0]
      [0 0 (* (+ far-plane near-plane) nf) (* 2 far-plane near-plane nf)]
      [0 0 -1 0]])))

(defn mtranslate [tx ty tz]
  [[1 0 0 tx]
   [0 1 0 ty]
   [0 0 1 tz]
   [0 0 0 1]])

(defn mrotate [theta-x theta-y theta-z]
  (let [cx (Math/cos theta-x)
        sx (Math/sin theta-x)
        cy (Math/cos theta-y)
        sy (Math/sin theta-y)
        cz (Math/cos theta-z)
        sz (Math/sin theta-z)]
    [[(* cy cz) (- (* cz sx sy) (* cx sz)) (+ (* sx sz) (* cx sy cz)) 0]
     [(* cy sz) (+ (* cx cz) (* sx sy sz)) (- (* cx sy sz) (* sx cz) ) 0]
     [(- sy) (* sx cy) (* cx cy) 0]
     [0 0 0 1]]))

(defn mscale [sx sy sz]
  [[sx 0 0 0]
   [0 sy 0 0]
   [0 0 sz 0]
   [0 0 0  1]])

;; shaders matrices are column major
;; this matrix is already column major
(defn ortho-mat [left right bottom top near far]
  [[(/ 2 (- right left))  0 0 0]
   [0 (/ 2 (- top bottom)) 0 0]
   [0 0 (/ 1 (- near far)) 0]
   [(/ (+ right left) (- left right)) (/ (+ top bottom) (- bottom top)) (/ near (- near far)) 1]]
  )
(def cube-verts
  (->
   ;; (cube [1 1 1] false)
   (sphere 0.5 100)
   (translate [0.5 0.5 0.5])
   get-mesh
   mesh->verts))

(defn draw [r]
  (let [

        verts cube-verts

        width 640
        height 640

        ctx (gpu/create-context)

        rot (* 2 r Math/PI 2)

        camera-mat (mat/mmul
                    (mtranslate 0 0 (+ -30 (* r 10)))
                    (mrotate 0 Math/PI 0))

        transform (mat/transpose
                   (mat/mmul
                    ;; projection
                    (perspective-fov (* 1/2 Math/PI) (/ width height) 0.1 500)
                    ;; camera
                    (mat/inverse camera-mat)

                    ;; model matrix
                    (mrotate 0 rot (/ rot 4))
                    (mscale 30 30 30)
                    (mtranslate -0.5 -0.5 -0.5)

                    ))
        uni-matrix
        (float-array
         (eduction
          cat
          transform))

        texture (gpu/load-texture ctx (io/file
                                       "/Users/adrian/workspace/clj-media/Clojure_logo.png"))
        sampler (gpu/create-sampler ctx)
        
        uni-buffer (gpu/create-buffer ctx {:usage #{:Uniform
                                                    :CopyDst}
                                           :type :f32
                                           :length (alength uni-matrix)})

        buf1 (gpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                     :type :f32
                                     :length (alength verts)
                                     })

        shader (gpu/create-shader ctx {:src shader-src})


        _ (gpu/copy-to-buffer ctx buf1 verts)
        _ (gpu/copy-to-buffer ctx uni-buffer uni-matrix)
        
        result (gpu/render ctx
                           {:vertex-entry-point "vs_main"
                            :fragment-entry-point "fs_main"
                            :shader shader
                            :bindings [uni-buffer
                                       buf1
                                       texture
                                       sampler]
                            :instance-count 1
                            :vertex-count (/ (alength verts) 3)
                            :width width
                            :height height})]
    ;; (gpu/save-png result (str "output-" r ".png"))
    result))

#_(defn add-gpu []
  (let [
        verts (-> my-shape
                  (translate [0 0 -20])
                  ;; (rotate [0 0 rot])
                  ;; (rotate [rot 0 0])
                  (get-mesh)
                  (mesh->verts))

        ;; verts (float-array [5.0, 5.0, -15.0])

        width 640
        height 640
        ctx (gpu/create-context)

        uni-matrix
        (float-array
         #_[1 0 0 100
          0 1 0 100
          0 0 1 100
          0 0 0 1]
         (eduction
            cat
            ;;(mat/transpose (mtranslate 100 100 100))
            (perspective-fov (* 1/2 Math/PI) (/ width height) 0.1 100)))
        #_(float-array [1 0 0 0
                      0 1 0 0
                      0 0 1 0
                      0 0 0 1])
        uni-buffer (gpu/create-buffer ctx {:usage #{:Uniform
                                                    :CopyDst}
                                           :type :f32
                                           :length (alength uni-matrix)})

        buf1 (gpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                     :type :f32
                                     :length (alength verts)
                                     })

        out-len (* 4 (quot (alength verts)
                           3))
        out (gpu/create-buffer ctx {:usage #{:Storage :CopySrc}
                                    :type :f32
                                    :length out-len})

        staging-buf (gpu/create-buffer ctx {:usage #{:MapRead :CopyDst}
                                            :type :f32
                                            :length out-len})

        shader (gpu/create-shader ctx {:src shader-src2})


        _ (gpu/copy-to-buffer ctx buf1 verts)
        _ (gpu/copy-to-buffer ctx uni-buffer uni-matrix)

        _ (gpu/compute ctx {:shader shader
                            :workgroups {:x (quot (alength verts) 3)}
                            :bindings [uni-buffer buf1 out]})

        _ (gpu/copy ctx out staging-buf)
        result (gpu/copy-from-buffer ctx staging-buf)]
    (clojure.pprint/pprint
     (->> (seq result)
          (partition-all 4)))))



(defn -main [& args]
  (let [num-frames 100]
    (gif/save-gif!
     (gif/graphics->media
      java2d/draw-to-graphics
      {:width 640
       :height 640}
      (eduction
       (map (fn [frameno]
              (ui/image
               (draw (double (* 2 (/ frameno num-frames)))))))
       (range num-frames)))
     "webgpu.gif"))
  
  ;; (draw 0)
  #_(doseq [r (range 0 500 5)]
    (draw (/ r 100.0)))
  ;; (add-gpu)
  )

