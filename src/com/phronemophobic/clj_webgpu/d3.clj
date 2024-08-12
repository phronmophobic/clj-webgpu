(ns com.phronemophobic.clj-webgpu.d3
  (:require [clojure.core.matrix :as mat]
            [clj-manifold3d.core :as mf]
            [clojure.math :as math]
            [clojure.java.io :as io]
            [com.phronemophobic.clogif :as gif]
            [membrane.ui :as ui]
            [membrane.java2d :as java2d]
            [com.phronemophobic.clj-webgpu.compute :as wgpu])
  (:import [manifold3d.glm DoubleVec3 DoubleVec2 DoubleMat4x3 DoubleMat3x2 IntegerVec4Vector DoubleMat4x3Vector
            DoubleVec3Vector DoubleVec4Vector IntegerVec3Vector IntegerVec3 MatrixTransforms DoubleVec4]
           java.awt.image.BufferedImage)
  (:gen-class))


(defn mesh->verts [^manifold3d.pub.DoubleMesh mesh]
  (let [pos (.vertPos mesh)
        buf (float-array
             (eduction
              (map (fn [^manifold3d.glm.IntegerVec3 idx]
                     (eduction
                      (map (fn [^manifold3d.glm.DoubleVec3 v]
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

(defn mesh->normals [^manifold3d.pub.DoubleMesh mesh]
  (let [normals (.vertNormal mesh)

        buf (if (seq normals)
              (float-array
               (eduction
                (map (fn [^manifold3d.glm.IntegerVec3 idx]
                       (eduction
                        (map (fn [^manifold3d.glm.DoubleVec3 v]
                               [(.x v)
                                (.y v)
                                (.z v)]))
                        [(.get normals (.x idx))
                         (.get normals (.y idx))
                         (.get normals (.z idx))])))

                cat
                cat
                (.triVerts mesh)))
              (float-array
               (eduction
                (map (fn [_]
                       1.0))
                (range (alength (mesh->verts mesh))))))]
    (identity mesh)
    buf))

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

(defn midentity []
  [[1 0 0 0]
   [0 1 0 0]
   [0 0 1 0]
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

(defn ortho-mat [left right bottom top near far]
  [[(/ 2 (- right left))  0 0 0]
   [0 (/ 2 (- top bottom)) 0 0]
   [0 0 (/ 1 (- near far)) 0]
   [(/ (+ right left) (- left right)) (/ (+ top bottom) (- bottom top)) (/ near (- near far)) 1]])


(defn pipeline-3d [{:keys [vertex-shader
                           fragment-shader]}]
  {:primitive {:topology :TriangleList
               :cullMode :Back}
   :vertex (merge
            {:entryPoint "vs_main"}
            vertex-shader)
   :fragment (merge
              {:entryPoint "fs_main"
               :targets [{:format :RGBA8UnormSrgb
                          :blend
                          {:color
                           {:srcFactor :SrcAlpha
                            :dstFactor :OneMinusSrcAlpha
                            :operation :Add}
                           :alpha
                           {:srcFactor :Zero
                            :dstFactor :One
                            :operation :Add}}
                          :writeMask :All}]}
              fragment-shader)
   :depthStencil {:stencilReadMask 0
                  :stencilWriteMask 0
                  :depthBias 0
                  :depthBiasSlopeScale 0
                  :depthBiasClamp 0
                  :depthWriteEnabled 1
                  :depthCompare :Less
                  :format :Depth24Plus
                  :stencilBack {:compare :Always
                                :failOp  :Keep
                                :depthFailOp :Keep
                                :passOp :Keep}
                  :stencilFront {:compare :Always
                                 :failOp  :Keep
                                 :depthFailOp :Keep
                                 :passOp :Keep}}
   :multisample {:count (int 1)
                 :mask (int -1)
                 :alphaToCoverageEnabled (int 0)}})


(defn render-pass-3d [{:keys [depth-texture-view texture-view]}]
  {:depthStencilAttachment
   {:depthClearValue 1.0
    :depthLoadOp :Clear
    :depthStoreOp :Store
    :view depth-texture-view}
   :colorAttachments
   [{:view texture-view
     :loadOp :Clear
     :resolveTarget nil
     :storeOp :Store
     :clearValue {:r 0.9
                  :g 0.1
                  :b 0.2
                  :a 1.0}}]})


(defn extents
  "Returns a map with :minx, :miny, :minz, :maxx, :maxy, and :maxz"
  [verts]
  (transduce
   (comp (partition-all 3))
   (completing
    (fn [{:keys [minx miny minz maxx maxy maxz]} [x y z]]
      {:minx (min minx x)
       :miny (min miny y)
       :minz (min minz z)
       :maxx (max maxx x)
       :maxy (max maxy y)
       :maxz (max maxz z)}))
   {:minx Double/POSITIVE_INFINITY
    :miny Double/POSITIVE_INFINITY
    :minz Double/POSITIVE_INFINITY

    :maxx Double/NEGATIVE_INFINITY
    :maxy Double/NEGATIVE_INFINITY
    :maxz Double/NEGATIVE_INFINITY}
   verts))

(defn center-camera [verts]
  (let [{:keys [minx miny minz maxx maxy maxz]} (extents verts)

        ;; find largest dim
        max-size (max (- maxx minx)
                      (- maxy miny)
                      (- maxz minz))
        scale (/ 1.0 max-size)

        mat
        #_(ortho-mat minx maxx miny maxy minz maxz)
        (mat/mmul
         ;; scale largest side to 1.0
         (mscale scale
                 scale
                 scale)
         ;; center at origin
         (mtranslate (- (- minx) (/ (- maxx minx) 2))
                     (- (- miny) (/ (- maxy miny) 2))
                     (- (- minz) (/ (- maxz minz) 2))))]
    mat))

(defn normalize-object-mat [verts]
  (let [{:keys [minx miny minz maxx maxy maxz]} (extents verts)

        ;; find largest dim
        max-size (max (- maxx minx)
                      (- maxy miny)
                      (- maxz minz))
        scale (/ 1.0 max-size)

        mat
        #_(ortho-mat minx maxx miny maxy minz maxz)
        (mat/mmul
         ;; scale largest side to 1.0
         (mscale scale
                 scale
                 scale)
         ;; center at origin
         (mtranslate (- (- minx) (/ (- maxx minx) 2))
                     (- (- miny) (/ (- maxy miny) 2))
                     (- (- minz) (/ (- maxz minz) 2))))]
    mat))

(defn draw-mesh [mesh rot]
  (let [width 640
        height 640

        verts (mesh->verts mesh)

        ctx (wgpu/create-context)

        shader (wgpu/create-shader ctx {:src (slurp (io/file "shaders/draw-mesh.wgsl"))})
        draw-figure-pipeline
        (wgpu/create-pipeline ctx
                              (pipeline-3d  {:fragment-shader {:module shader}
                                             :vertex-shader {:module shader}})
                              )

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

        model-mat (mat/mmul
                   (apply mrotate rot)
                   (normalize-object-mat verts))

        transform (mat/transpose
                   (mat/mmul
                    (perspective-fov (* 1/2 Math/PI) 1 0.1 20)
                    (mtranslate 0 0 -2)                   
                    
                    model-mat))

        light-mat (mat/mmul
                   model-mat)


        uni-matrix
        (float-array
         (eduction
          cat
          transform))

        uni (wgpu/create-buffer ctx {:usage #{:Uniform
                                              :CopyDst}
                                     :type :f32
                                     :length 16})

        _ (wgpu/copy-to-buffer ctx uni uni-matrix)

        uni2-matrix
        (float-array
         (eduction
          cat
          (mat/transpose
           light-mat)))

        uni2 (wgpu/create-buffer ctx {:usage #{:Uniform
                                               :CopyDst}
                                      :type :f32
                                      :length 16})

        _ (wgpu/copy-to-buffer ctx uni2 uni2-matrix)

        texture (wgpu/load-texture ctx (io/file
                                        "/Users/adrian/workspace/clj-media/Clojure_logo.png"))
        sampler (wgpu/create-sampler ctx)
        
        buf1 (wgpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                      :type :f32
                                      :length (alength verts)})
        
        _ (wgpu/copy-to-buffer ctx buf1 verts)

        normals (mesh->normals mesh)
        normal-buf (wgpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                            :type :f32
                                            :length (alength normals)})
        _ (wgpu/copy-to-buffer ctx normal-buf normals)

        draw-figure
        {
         :render-pass
         (render-pass-3d {:texture-view offscreen-texture-view
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


        _ (wgpu/submit
           ctx
           [#_(compute-pass
               {:bindings [uni buf2 input-texture]})

            draw-figure
            ])

        ;; wait for queue to 
        _ (wgpu/poll (:device ctx) true)

        buf (wgpu/save-texture ctx offscreen-texture width height)
        img (wgpu/->buffered-image  buf width height)]
    img)
  )

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
              (let [rot (* 0.5 Math/PI
                           (/ frameno num-frames))]
                  (ui/image
                   (draw-mesh mesh [rot (/ rot 2) (/ rot 4)] )))))
       (range num-frames)))
     "webgpu.gif"))
  )

(defn -main [& args]
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
       (draw-mesh (mf/import-mesh fname))
       "draw-mesh.png"))

  ,)

(defn ^:private  apply-transform [mat verts]
  (let [pts (mat/transpose
             (into []
                   (comp (partition-all 3)
                         (map (fn [[x y z]]
                                [x y z 1.0])))
                   verts))
        [xs ys zs ws] (mat/mmul mat pts)]
    (clojure.pprint/pprint
     [xs
      ys
      zs
      (mapv / zs ws)]
     )))

(comment
  (def my-mesh
    (-> (mf/import-mesh "/Users/adrian/Downloads/36ee14ff-06a6-47e1-9601-cbd3daf2e491.glb")))
  (def verts (mesh->verts my-mesh))
  (wgpu/save-png
   (draw-mesh my-mesh)
   "draw-mesh.png")

  (center-camera
   (-> (mesh->verts (cube 10 20 30))
       
       
       )

   (defn )

   (-> (mf/cube 10 10 10)
       mf/get-mesh
       (mesh->verts))

   )
  (animate (-> (mf/cube 10 10 10)
               mf/get-mesh
       ))

  (clojure.pprint/pprint
   (let [verts (-> (mf/cube 10 20 30)
                   mf/get-mesh
                   (mesh->verts))]
     (apply-transform
      (center-camera verts)
      verts)))
  ,)

(defonce running? (atom false))

(defn java2d++ [mesh]
  (let [width 640
        height 640

        verts (mesh->verts mesh)
        ctx (wgpu/create-context)

        shader (wgpu/create-shader ctx {:src (slurp (io/file "shaders/draw-mesh.wgsl"))})
        draw-figure-pipeline
        (wgpu/create-pipeline ctx
                              (pipeline-3d  {:fragment-shader {:module shader}
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

        normals (mesh->normals mesh)
        normal-buf (wgpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                            :type :f32
                                            :length (alength normals)})
        _ (wgpu/copy-to-buffer ctx normal-buf normals)

        draw-figure
        {
         :render-pass
         (render-pass-3d {:texture-view offscreen-texture-view
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
                              
                              (mtranslate 0 0 -2)
                              (mrotate 0 Math/PI 0)))
        

        winfo (java2d/run
                (fn []
                  (when-let [[_ new] @imgs*]
                    (ui/on
                     :key-press
                     (fn [s]
                       (prn s)
                       (case s
                         "s" (vswap! user-mat* #(mat/mmul (mtranslate 0 0 -0.2)
                                                          %))
                         "a" (vswap! user-mat* #(mat/mmul (mrotate 0 (/ Math/PI -10) 0)
                                                          %))
                         "d" (vswap! user-mat* #(mat/mmul (mrotate 0 (/ Math/PI 10) 0)
                                                          %))
                         "w" (vswap! user-mat* #(mat/mmul (mtranslate 0 0 0.2)
                                                          %))
                         :escape (reset! running? false)

                         
                         
                         nil)
                       
                       
                       nil)
                     (ui/image new)))))
        repaint (::java2d/repaint winfo)

        model-init (normalize-object-mat verts)
        draw-frame
        (fn [r]
          (let [rot (* 0.5 Math/PI r)
                model-mat (mat/mmul
                           (mrotate rot (/ rot 2) (/ rot 4))
                           model-init)

                transform (mat/transpose
                           (mat/mmul
                            (perspective-fov (* 1/2 Math/PI) 1 0.1 20)
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
                   model-mat)))
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
             (mf/sphere 3 100)
             mf/get-mesh
             ))
  )
