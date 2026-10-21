(ns com.phronemophobic.clj-webgpu.d3
  (:require [clojure.core.matrix :as mat]
            [com.phronemophobic.clj-webgpu.d3.matrix-util :as mutil]
            [clj-manifold3d.core :as mf]
            [clojure.math :as math]
            [clojure.java.io :as io]

            
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]

            [com.phronemophobic.clogif :as gif]
            [membrane.ui :as ui]
            [membrane.skia :as skia]
            [com.phronemophobic.clj-webgpu.impl.shader :as shader]
            [com.phronemophobic.clj-webgpu.compute :as wgpu])
  (:import [manifold3d.linalg DoubleVec3 DoubleVec2 IntegerVec4Vector 
            DoubleVec3Vector DoubleVec4Vector IntegerVec3Vector IntegerVec3 MatrixTransforms DoubleVec4]
           ;;java.awt.image.BufferedImage
           )
  (:gen-class))


(defn mesh->verts [mesh]
  (let [pos (.vertProperties mesh)
        buf (float-array
             (eduction
              ;; (map (fn [ idx]
              ;;        (eduction
              ;;         (map (fn [v]
              ;;                [(.x v)
              ;;                 (.y v)
              ;;                 (.z v)]))
              ;;         [(.get pos (.x idx))
              ;;          (.get pos (.y idx))
              ;;          (.get pos (.z idx))])))

              ;; cat
              ;; cat

              
              (mapcat (fn [i]
                        (eduction
                         (map (fn [j]
                                (.get pos (+ (* i 3)
                                             j))))
                         (range 3))))
              (.toIntArray (.triVerts mesh))))]
    (identity mesh)
    buf))

(defn triangle-normal
  "Calculate the normal of a triangle given its vertices."
  [[v1 v2 v3]]
  (let [edge1 (mat/sub v2 v1)
        edge2 (mat/sub v3 v1)
        normal (mat/cross edge1 edge2)]
    (mat/normalise normal)))



(defn mesh->normals2 [mesh]
  "Calculate normals for a sequence of triangles."
  [mesh]
  (let [pos (.vertProperties mesh)
        normals
        (float-array
         (eduction
          #_(map #(.get pos %))
          (mapcat (fn [i]
                    (eduction
                     (map (fn [j]
                            (.get pos (+ (* i 3)
                                         j))))
                     (range 3))))
          (partition-all 3)
          (partition-all 3)
          ;; off by one error sometimes?
          ;; not sure whose fault it is
          (filter #(= 3 (count %)))
          (mapcat (fn [triangle]
                    (let [norm (triangle-normal triangle)]
                      [norm norm norm])))
          cat
          (.toIntArray (.triVerts mesh)))
         #_(eduction
            (map (fn [idx]
                   (eduction
                    (map (fn [v]
                           [(.x v)
                            (.y v)
                            (.z v)]))
                    [(.get pos (.x idx))
                     (.get pos (.y idx))
                     (.get pos (.z idx))])))
            (mapcat (fn [triangle]
                      ;; use same normal for all verts
                      (let [norm (triangle-normal triangle)]
                        [norm norm norm])))
            cat
            (.triVerts mesh)))]
    (identity pos)
    (identity mesh)
    normals))


(defn mesh->normals [mesh]
  (let [ ;; normals (.vertNormal mesh)

        ;; buf (if (seq normals)
        ;;       (float-array
        ;;        (eduction
        ;;         (map (fn [ idx]
        ;;                (eduction
        ;;                 (map (fn [ v]
        ;;                        [(.x v)
        ;;                         (.y v)
        ;;                         (.z v)]))
        ;;                 [(.get normals (.x idx))
        ;;                  (.get normals (.y idx))
        ;;                  (.get normals (.z idx))])))

        ;;         cat
        ;;         cat
        ;;         (.triVerts mesh)))
        ;;       (mesh->normals2 mesh))
        buf (mesh->normals2 mesh)
        ]
    (identity mesh)
    buf))



(defn pipeline-3d [{:keys [vertex-shader
                           fragment-shader]}]
  {:primitive {:topology :TriangleList
               ;; :cullMode :Front
               :cullMode :Back
               ;; :cullMode :None
               }
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
     :resolveTarget 0
     :storeOp :Store
     :clearValue 
     {:r 0
      :g 0
      :b 0
      :a 1}
     #_{:r 0.9
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
         (mutil/scale scale
                      scale
                      scale)
         ;; center at origin
         (mutil/translate (- (- minx) (/ (- maxx minx) 2))
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
         (mutil/scale scale
                      scale
                      scale)
         ;; center at origin
         (mutil/translate (- (- minx) (/ (- maxx minx) 2))
                          (- (- miny) (/ (- maxy miny) 2))
                          (- (- minz) (/ (- maxz minz) 2))))]
    mat))

(defn draw-mesh [mesh rot]
  (let [width 640
        height 640

        verts (if (instance? (class (float-array 0)) mesh)
                mesh
                (mesh->verts mesh))

        ctx (wgpu/create-context)

        shader (wgpu/create-shader ctx
                                   {:src 
                                    #_(slurp
                                     (io/resource "com/phronemophobic/clj_webgpu/shaders/model_lighting.wgsl"))
                                    (shader/preprocess-shader
                                     (io/file "../lygia")
                                     (slurp
                                      "../clj-webgpu/shaders/default.wgsl"))})
        draw-figure-pipeline
        (wgpu/create-render-pipeline ctx
                                     (pipeline-3d  {:fragment-shader {:module shader}
                                                    :vertex-shader {:module shader}})
                                     )
        offscreen-texture (wgpu/create-texture
                           ctx
                           {
                            :dimension :3D
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
                   (apply mutil/rotate rot)
                   (mutil/rotate 1 0 0) 
                   (normalize-object-mat verts))
        
        
        camera-mat (mat/mmul
                    (mutil/perspective-fov (* 1/2 Math/PI) 1 0.1 20)
                    (mutil/translate 0 0 -1))

        light-mat (do
                    (apply mutil/rotate rot))

        camera-matrix-uni (wgpu/create-buffer ctx {:usage #{:Uniform
                                                            :CopyDst}
                                                   :type :f32
                                                   :length 16})

        _ (wgpu/copy-to-buffer ctx
                               camera-matrix-uni
                               (dtype/make-container :native-heap
                                                     :float32 
                                                     (into []
                                                           cat
                                                           (mat/transpose
                                                            camera-mat))))
        
        model-matrix-uni (wgpu/create-buffer ctx {:usage #{:Uniform
                                                           :CopyDst}
                                                  :type :f32
                                                  :length 16})
        
        _ (wgpu/copy-to-buffer ctx
                               model-matrix-uni
                               (dtype/make-container :native-heap
                                                     :float32 
                                                     (into []
                                                           cat
                                                           (mat/transpose
                                                            model-mat))))

        uni2-matrix
        (dtype/make-container :native-heap
                              :float32
                              (into []
                                    cat
                                    (mat/transpose
                                     light-mat)))

        uni2 (wgpu/create-buffer ctx {:usage #{:Uniform
                                               :CopyDst}
                                      :type :f32
                                      :length 16})

        _ (wgpu/copy-to-buffer ctx uni2 uni2-matrix)

        texture (wgpu/load-texture ctx (ui/image "/Users/adrian/workspace/clj-media/Clojure_logo.png"))

        sampler (wgpu/create-sampler ctx)

        buf1 (wgpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                      :type :f32
                                      :length (alength verts)})
        
        

        _ (wgpu/copy-to-buffer ctx buf1 (dtype/make-container :native-heap :float32 verts))

        normals (mesh->normals2 mesh)
        normal-buf (wgpu/create-buffer ctx {:usage #{:Storage :CopyDst}
                                            :type :f32
                                            :length (alength normals)})
        _ (wgpu/copy-to-buffer ctx normal-buf (dtype/make-container :native-heap :float32 normals))
        draw-figure
        {
         :render-pass
         (render-pass-3d {:texture-view offscreen-texture-view
                          :depth-texture-view depth-texture-view})
         
         :draws [{:vertex-count (/ (alength verts) 3)
                  :pipeline draw-figure-pipeline
                  :bindings [
                             camera-matrix-uni
                             buf1
                             normal-buf
                             texture
                             sampler
                             uni2
                             model-matrix-uni
                             ]}]
         }
        _ (do (prn "submitting...")
              (flush))

        _ (wgpu/submit
           ctx
           [#_(compute-pass
               {:bindings [uni buf2 input-texture]})

            draw-figure
            ])
        _ (do (prn "we submitted!")
              (flush)) 

        ;; wait for queue to 
        _ (wgpu/poll (:device ctx) true)
        _ (do (prn "now save the texture...")
              (flush))

        buf (wgpu/save-texture2 ctx nil offscreen-texture width height)]
    buf))





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
   (-> (draw-mesh my-mesh)
       wgpu/->buffered-image)
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






(comment
  (def my-mesh (mf/import-mesh "/Users/adrian/Downloads/Klein_Smoother_no_base.STL"))
  (animate my-mesh)
  ,)
(defn -main [& args]
  
  (let [;; mesh (mf/import-mesh "/Users/adrian/Downloads/36ee14ff-06a6-47e1-9601-cbd3daf2e491.glb")
        mesh (mf/import-mesh "/Users/adrian/Downloads/Klein_Smoother_no_base.STL")
        #_#_mesh (-> (mf/difference
                      (mf/cube [5 5 5] true)
                      (mf/cylinder 10
                                   5))
                     mf/get-mesh)
        
        _ (wgpu/create-context)
        native-buf (time (draw-mesh mesh [0 0 0]))
        [w h] [640 640]
        buf (byte-array (native-buffer/native-buffer-byte-len native-buf))
        _ (dtype/copy! native-buf buf)
        pm (skia/pixmap 0 buf w h  skia/kRGBA_8888_SkColorType skia/kUnpremul_SkAlphaType (* w 4))]
    (skia/save-image "doesitwork2.png" pm)
    )
  (prn "done"))


(comment
  (-main)
  
  (defn perspective-camera [{:keys [near far fov aspect]}])
  (defn directional-light [{:keys [pos color intensity]}])
  
  ;; shaders namespace with include support wgsl
  
  
  (render
   {:camera (perspective-camera {})
    ;; :lights [(directional-light {})]
    :scene
    [{:mesh (mesh)
      :material (pbr)}
     (cube)]})
  ,)

(defn vert-init! [])
(defn vert-update-camera [])
(defn vert-update-model [])
(defn vert-draw! [{}])

(defn init! [{:keys [width height] :as ctx}]
  (let [gpu-context (wgpu/create-context)
        offscreen-texture (wgpu/create-texture
                           gpu-context
                           {
                            :dimension :3D
                            :size {:width width
                                   :height height
                                   :depthOrArrayLayers 1}
                            :format :RGBA8UnormSrgb
                            :mipLevelCount 1
                            :sampleCount 1
                            :usage #{:RenderAttachment :CopySrc}})
        offscreen-texture-view (wgpu/create-texture-view
                                offscreen-texture
                                {:baseArrayLayer 0
                                 :arrayLayerCount 1
                                 :baseMipLevel 0
                                 :mipLevelCount 1
                                 :aspect :All})
        
        mesh-shader (wgpu/create-shader gpu-context
                                        {:src 
                                         #_(slurp
                                            (io/resource "com/phronemophobic/clj_webgpu/shaders/model_lighting.wgsl"))
                                         (shader/preprocess-shader
                                          (io/file "../lygia")
                                          (slurp
                                           "../clj-webgpu/shaders/default2.wgsl"))})
        
        depth-texture (wgpu/create-texture
                       gpu-context
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
          :format :Depth24Plus})]
    (merge
     ctx
     {:gpu-context gpu-context
      :uniform {:camera (wgpu/create-buffer gpu-context
                                            {:usage #{:Uniform
                                                      :CopyDst}
                                             :type :f32
                                             :length 16})
                :model (wgpu/create-buffer gpu-context {:usage #{:Uniform
                                                                 :CopyDst}
                                                        :type :f32
                                                        :length 16})}
      :render-pass (render-pass-3d {:texture-view offscreen-texture-view
                                    :depth-texture-view depth-texture})
      :texture {:screen offscreen-texture
                :depth depth-texture}
      :texture-view {:screen offscreen-texture-view
                     :depth depth-texture-view}
      
      
      :shader {:mesh mesh-shader}
      
      
      :pipeline {:mesh (wgpu/create-render-pipeline gpu-context
                                                    (pipeline-3d  {:fragment-shader {:module mesh-shader}
                                                                   :vertex-shader {:module mesh-shader}}))}})))


(defn check-init [_]
  (time  (init! {:width 100 :height 100}))
  (System/gc)
  (println "done."))


(defn draw! [ctx scene] 
  
  (let [camera-uniform (-> ctx :uniform :camera)
        gpu-context (:gpu-context ctx)
        _ (wgpu/copy-to-buffer gpu-context
                               camera-uniform
                               (dtype/make-container :native-heap
                                                     :float32 
                                                     (into []
                                                           cat
                                                           (mat/transpose
                                                            (:camera scene)))))

        
        [ctx gpu-draws]
        (loop [ctx ctx
               draws (seq (:draw scene))
               gpu-draws []]
          (if (not draws)
            [ctx gpu-draws]
            (let [draw (first draws)
                  
                  ;; assume mesh for now
                  {:keys [mesh id model-matrix]} draw
                  
                  {:keys [model-uniform
                          vertex-buffer
                          normal-buffer
                          vertex-count]
                   :as instance} 
                  (if-let [instance (get-in ctx [:instance id])]
                    instance
                    (let [verts (if (instance? (class (float-array 0)) mesh)
                                  mesh
                                  (mesh->verts mesh))
                          vertex-buffer (wgpu/create-buffer gpu-context
                                                            {:usage #{:Storage :CopyDst}
                                                             :type :f32
                                                             :length (alength verts)})
                          _ (wgpu/copy-to-buffer gpu-context vertex-buffer (dtype/make-container :native-heap :float32 verts))
                          
                          normals (mesh->normals2 mesh)
                          normal-buffer (wgpu/create-buffer gpu-context
                                                            {:usage #{:Storage :CopyDst}
                                                             :type :f32
                                                             :length (alength normals)})
                          _ (wgpu/copy-to-buffer gpu-context normal-buffer (dtype/make-container :native-heap :float32 normals))
                          
                          model-uniform (wgpu/create-buffer gpu-context
                                                            {:usage #{:Uniform
                                                                      :CopyDst}
                                                             :type :f32
                                                             :length 16})]
                      
                      
                      {:vertex-count (quot (alength verts) 3)
                       :vertex-buffer vertex-buffer
                       :normal-buffer normal-buffer
                       :model-uniform model-uniform}))
                  ctx (assoc-in ctx [:instance id] instance)
                  
                  model-matrix (:model-matrix draw)
                  _ (wgpu/copy-to-buffer gpu-context
                                         model-uniform
                                         (dtype/make-container :native-heap
                                                               :float32 
                                                               (into []
                                                                     cat
                                                                     (mat/transpose
                                                                      (or model-matrix
                                                                          (mutil/identity))))))
                  
                  
                  gpu-draw {:vertex-count vertex-count
                            :pipeline (-> ctx :pipeline :mesh)
                            :bindings [camera-uniform
                                       vertex-buffer
                                       normal-buffer
                                       model-uniform
                                       
                                       ]}]
              (recur ctx (next draws) (conj gpu-draws gpu-draw)))))
        
        draw-pass {:render-pass (:render-pass ctx)
                   :draws gpu-draws}]
    (wgpu/submit gpu-context
                 [draw-pass])
    ctx))

(defn ctx->pixmap [ctx pm]
  (let [{:keys [gpu-context width height]} ctx
        offscreen-texture (-> ctx :texture :screen)
        native-buf (:native-buf pm)
        native-buf (wgpu/save-texture2 gpu-context native-buf offscreen-texture width height)

        pm (if pm
             (update pm :id inc)
             (-> (skia/pixmap 0 
                              (-> native-buf dt-ffi/->pointer .address )
                              #_(byte-array (native-buffer/native-buffer-byte-len native-buf))
                              width height skia/kRGBA_8888_SkColorType skia/kUnpremul_SkAlphaType (* width 4))
                 (assoc :native-buf native-buf)))

        ;;_ (dtype/copy! native-buf (:buf pm))
        ]
    pm))

(defn check-draw [_]
  (let [ctx (init! {:width 640 :height 640})
        
        my-mesh (mf/import-mesh "/Users/adrian/Downloads/Klein_Smoother_no_base.STL")
        verts (mesh->verts my-mesh)

        ctx (draw! 
             ctx
             {:camera (mat/mmul
                       (mutil/perspective-fov (* 1/2 Math/PI) 1 0.1 20)
                       (mutil/translate 0 0 -1))
              :draw 
              (into []
                    (map (fn [i]
                           {:id (random-uuid)
                            :type :mesh
                            :mesh my-mesh
                            :model-matrix
                            (mat/mmul
                             (mutil/translate i 0 (* 2 i))
                             (mutil/rotate 1 0 0)
                             (normalize-object-mat verts)
                             )}))
                    (range 10))
              #_[{:id (random-uuid)
                      :type :mesh
                      :mesh my-mesh
                      :model-matrix
                      (mat/mmul
                       #_(mutil/rotate 1 0 0)
                       (mutil/translate 0 0 -10)
                       (normalize-object-mat verts))}
                     
                     {:id (random-uuid)
                      :type :mesh
                      :mesh my-mesh
                      :model-matrix
                      (mat/mmul
                       #_(mutil/rotate 1 0 0)
                       (mutil/translate 0 -0.5 0)
                       (normalize-object-mat verts))}
                     
                     {:id (random-uuid)
                      :type :mesh
                      :mesh my-mesh
                      :model-matrix
                      (mat/mmul
                       #_(mutil/rotate 1 0 0)
                       (mutil/translate -0.50 -0.5 0)
                       (normalize-object-mat verts))}]})
        
        ;; _ (wgpu/poll (:device ctx) true)
        
        pm (ctx->pixmap ctx nil)]
    (skia/save-image "woohoo.png" pm)
    (prn "done")))


(defn check-redraw [_]
  (let [width 1024
        height 640
        ctx (init! {:width width :height height})
        
        my-mesh (-> #_(mf/sphere 1 100)
                    (mf/cube 1 1 1 true)
                    mf/get-mesh)
        #_(mf/import-mesh "/Users/adrian/Downloads/Klein_Smoother_no_base.STL")
        verts (mesh->verts my-mesh)

        pm* (volatile! (ctx->pixmap ctx nil))
        ctx* (volatile! ctx)
        camera* (volatile! 
                 (#_mutil/perspective-fov3-reverseZ
                  mutil/perspective-fov
                   (/ (* Math/PI 45) 180)
                   
                   (/ width height)
                   0.1
                   1000)
                            
                            #_(mutil/translate 0 0 -1))
        view-fn (fn []
                  (time
                   (do
                     (vswap! ctx* draw!
                             {:camera @camera*
                              :draw 
                              [{:id [:mesh 0]
                                :type :mesh
                                :mesh my-mesh
                                :model-matrix
                                (mat/mmul
                                 (mutil/translate 0 0 -10)
                                 ;; (mutil/rotate 1 0 0)
                                 #_(normalize-object-mat verts)
                                 )}
                               {:id [:mesh 1]
                                :type :mesh
                                :mesh (-> (mf/sphere 1 100)
                                          mf/get-mesh)
                                :model-matrix
                                (mat/mmul
                                 (mutil/translate 0 3 -10)
                                 ;; (mutil/rotate 1 0 0)
                                 #_(normalize-object-mat verts)
                                 )}
                               {:id [:mesh 2]
                                :type :mesh
                                :mesh (-> (mf/difference (mf/cylinder 3 1 1 100 true)
                                                         (mf/sphere 1.2 100 ))
                                          mf/get-mesh)
                                :model-matrix
                                (mat/mmul
                                 (mutil/translate -3 0 -10)
                                 ;; (mutil/rotate 1 0 0)
                                 #_(normalize-object-mat verts)
                                 )}]
                                    
                                    })
                     (let [pm (vreset! pm* (ctx->pixmap @ctx* @pm*))]
                       (ui/on
                        :key-press
                        (fn [s]
                          (prn s)
                          (let [dist 0.12]
                            (case s
                              ("w" :kp_8) (vswap! camera* #(mat/mmul 
                                                            %
                                                            (mutil/translate 0 dist 0)
                                                      ))
                              ("s" :kp_2) (vswap! camera* #(mat/mmul 
                                                            %
                                                            (mutil/translate 0 (- dist) 0)))

                              :left (vswap! camera* #(mat/mmul 
                                                      %
                                                      (mutil/translate (- dist) 0 0)
                                                      ))
                              :right (vswap! camera* #(mat/mmul
                                                       %
                                                       (mutil/translate dist 0 0)
                                                       ))
                              :up (vswap! camera* #(mat/mmul 
                                                    %
                                                    (mutil/translate 0 0 dist)
                                                    
                                                    ))
                              :down (vswap! camera* #(mat/mmul 
                                                      %
                                                      (mutil/translate 0 0 (- dist))
                                                      
                                                      ))
                              nil)))
                        pm)))))
        ]
    (skia/run-sync view-fn
                   {:window-start-width width
                    :window-start-height height})

    (prn "done")
    (shutdown-agents)))



