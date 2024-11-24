(ns com.phronemophobic.clj-webgpu.d3.java2d
  (:require [clojure.core.matrix :as mat]
            [com.phronemophobic.clj-webgpu.d3.matrix-util :as mutil]
            [clj-manifold3d.core :as mf]
            [clojure.math :as math]
            [clojure.java.io :as io]
            [com.phronemophobic.clogif :as gif]
            [membrane.ui :as ui]
            [membrane.java2d :as java2d]
            [com.phronemophobic.clj-webgpu.d3 :as d3]
            [com.phronemophobic.clj-webgpu.compute :as wgpu])
  (:import [manifold3d.glm DoubleVec3 DoubleVec2 DoubleMat4x3 DoubleMat3x2 IntegerVec4Vector DoubleMat4x3Vector
            DoubleVec3Vector DoubleVec4Vector IntegerVec3Vector IntegerVec3 MatrixTransforms DoubleVec4]
           java.awt.image.BufferedImage)
  (:gen-class))

(defonce running? (atom false))


(defn animate [mesh]
  (let [num-frames 100
        ]
    (gif/save-gif!
     (gif/graphics->media
      java2d/draw-to-graphics
      {:width 640
       :height 640}
      (eduction
       (map (fn [frameno]
              (let [rot (* 0.5 4 Math/PI
                           (/ frameno num-frames))]
                (ui/image
                 (-> (d3/draw-mesh mesh [rot (/ rot 2) (/ rot 4)] )
                     (wgpu/->buffered-image))))))
       (range num-frames)))
     "webgpu.gif"))
  )



(defn java2d++ [mesh]
  (let [width 640
        height 640

        verts (d3/mesh->verts mesh)
        ctx (wgpu/create-context)

        shader (wgpu/create-shader ctx {:src (slurp (io/resource "com/phronemophobic/clj_webgpu/shaders/model_lighting.wgsl"))})
        draw-figure-pipeline
        (wgpu/create-pipeline ctx
                              (d3/pipeline-3d  {:fragment-shader {:module shader}
                                                :vertex-shader {:module shader}}))

        offscreen-texture (wgpu/create-texture
                           ctx
                           {
                            :dimension :2D
                            :size {:width width
                                   :height height
                                   :depthOrArrayLayers 1}
                            :format :RGBA8UnormSrgb
                            :mipLevelCount 1
                            :sampleCount 1
                            :usage #{:RenderAttachment :CopySrc}})

        offscreen-texture-view (wgpu/create-texture-view
                                offscreen-texture
                                {
                                 :baseArrayLayer 0
                                 :arrayLayerCount 1
                                 :baseMipLevel 0
                                 :mipLevelCount 1
                                 :aspect :All})

        depth-texture (wgpu/create-texture
                       ctx
                       {
                        :size {:width width
                               :height height
                               :depthOrArrayLayers 1}
                        :mipLevelCount 1
                        :sampleCount 1
                        :dimension :2D
                        :format :Depth24Plus
                        :usage #{:RenderAttachment}})

        depth-texture-view
        (wgpu/create-texture-view
         depth-texture
         {
          :aspect :DepthOnly
          :baseArrayLayer 0
          :arrayLayerCount 1
          :baseMipLevel 0
          :mipLevelCount 1
          :format :Depth24Plus})



        uni (wgpu/create-buffer ctx {:usage #{:Uniform
                                              :CopyDst}
                                     :type :f32
                                     :length 16})





        uni2 (wgpu/create-buffer ctx {:usage #{:Uniform
                                               :CopyDst}
                                      :type :f32
                                      :length 16})



        texture (wgpu/load-texture ctx (io/file
                                        "/Users/adrian/workspace/clj-media/Clojure_logo.png"))
        sampler (wgpu/create-sampler ctx)
        
        buf1 (wgpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                      :type :f32
                                      :length (alength verts)})
        
        _ (wgpu/copy-to-buffer ctx buf1 verts)

        normals (d3/mesh->normals mesh)
        normal-buf (wgpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                            :type :f32
                                            :length (alength normals)})
        _ (wgpu/copy-to-buffer ctx normal-buf normals)

        draw-figure
        {:render-pass
         (d3/render-pass-3d {:texture-view offscreen-texture-view
                             :depth-texture-view depth-texture-view})
         
         :draws [{:vertex-count (/ (alength verts) 3)
                  :pipeline draw-figure-pipeline
                  :bindings [
                             uni
                             buf1
                             normal-buf
                             texture
                             sampler
                             uni2
                             ]}]
         }


        
        imgs* (volatile! [(BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
                          (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)])

        user-mat* (volatile! (mat/mmul
                              (mutil/translate 0 0 -2)
                              (mutil/rotate 0 Math/PI 0)))
        

        winfo (java2d/run
                (fn []
                  (when-let [[_ new] @imgs*]
                    (ui/on
                     :key-press
                     (fn [s]
                       (case s
                         "s" (vswap! user-mat* #(mat/mmul (mutil/translate 0 0 -0.2)
                                                          %))
                         "a" (vswap! user-mat* #(mat/mmul (mutil/rotate 0 (/ Math/PI -10) 0)
                                                          %))
                         "d" (vswap! user-mat* #(mat/mmul (mutil/rotate 0 (/ Math/PI 10) 0)
                                                          %))
                         "w" (vswap! user-mat* #(mat/mmul (mutil/translate 0 0 0.2)
                                                          %))
                         :escape (reset! running? false)

                         
                         
                         nil)
                       
                       
                       nil)
                     (ui/image new)))))
        repaint (::java2d/repaint winfo)

        model-init (d3/normalize-object-mat verts)
        draw-frame
        (fn [r]
          (let [rot (* 0.5 Math/PI r)
                model-mat (mat/mmul
                           (mutil/rotate rot (/ rot 2) (/ rot 4))
                           model-init)

                transform (mat/transpose
                           (mat/mmul
                            (mutil/perspective-fov (* 1/2 Math/PI) 1 0.1 20)
                            @user-mat*
                            
                            model-mat))

                uni-matrix
                (float-array
                 (eduction
                  cat
                  transform))
                _ (wgpu/copy-to-buffer ctx uni uni-matrix)


                uni2-matrix
                (float-array
                 (eduction
                  cat
                  (mat/transpose
                   (mutil/rotate rot (/ rot 2) (/ rot 4))

                   ;; scaling messes up normals
                   ;;model-mat
                   )))
                _ (wgpu/copy-to-buffer ctx uni2 uni2-matrix)

                _ (wgpu/submit
                   ctx
                   [#_(compute-pass
                       {:bindings [uni buf2 input-texture]})

                    draw-figure
                    ])

                ;; wait for queue to 
                _ (wgpu/poll (:device ctx) true)

                buf (wgpu/save-texture ctx offscreen-texture width height)
                [next current] @imgs*
                new-img (wgpu/->buffered-image next buf width height)]

            (vreset! imgs* [current next])
            (repaint)))]
    (reset! running? true )
    (loop [r 0.0]
      (when (and (not (Thread/interrupted))
                 @running?)
        (draw-frame r)
        (Thread/sleep 10)
        (recur (+ r 0.01))))))


(defn -main [& args]
  (java2d++ (->
             ;; (mf/cube 1 1 1)
             ;; (mf/sphere 3 100)

             ;; (mf/text "/Library/Fonts/SF-Pro-Text-Regular.otf" "λ")
             ;; (mf/scale-to-height 3)
             ;; (mf/extrude 1)

             ;; mf/get-mesh

             (mf/import-mesh "/Users/adrian/Downloads/Klein_Smoother_no_base.STL")
             )))


#_(defn -main [& args]
  (let [fname (or (first args)
                  "/Users/adrian/Downloads/36ee14ff-06a6-47e1-9601-cbd3daf2e491.glb")
        mesh (mf/import-mesh fname)]
    (animate mesh
     #_(-> (mf/loft
          (let [dx (/ 1 10)]
            (reductions
             (fn [m i]
               (let [x1 (* i dx)
                     x2 (+ x1 dx)
                     y1 (Math/sin x1)
                     y2 (Math/sin x2)
                     dy (- y2 y1)
                     dist (Math/sqrt (+ (* dx dx)
                                        (* dy dy)))
                     angle (Math/atan2 dy dx)
                     ]
                 (prn x1 y1 dist angle)
                 (assoc m
                        :frame (-> (:frame m)
                                   (MatrixTransforms/Yaw angle)
                                   (MatrixTransforms/Translate (DoubleVec3. 0 0 dist))
                                   (MatrixTransforms/Yaw (- angle))
                                   ))))
             {:cross-section (mf/circle 0.1 10)
              ;; :algorithm :eager-nearest-neighbor ;; Optional algorithm specifier in first map.
              :frame (mf/frame 1)}
             (range 0 200))))
         mf/get-mesh)
     #_(->
        ;; (mf/cube 1 1 1)
        (mf/sphere 3 100)
        mf/get-mesh
        )
     )
    #_(wgpu/save-png
       (-> (draw-mesh (mf/import-mesh fname))
           (wgpu/->buffered-image))
       "draw-mesh.png"))

  ,)
