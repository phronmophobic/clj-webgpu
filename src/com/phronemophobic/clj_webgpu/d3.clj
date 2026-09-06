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

        shader (wgpu/create-shader ctx {:src (slurp (io/resource "com/phronemophobic/clj_webgpu/shaders/model_lighting.wgsl"))})
        draw-figure-pipeline
        (wgpu/create-pipeline ctx
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
                   (normalize-object-mat verts))

        transform (mat/transpose
                   (mat/mmul
                    (mutil/perspective-fov (* 1/2 Math/PI) 1 0.1 20)
                    (mutil/translate 0 0 -1)
                    
                    model-mat))

        light-mat (do
                    (apply mutil/rotate rot))


        uni-matrix
        (dtype/make-container :native-heap
                              :float32 
                              (into []
                                    cat
                                    transform))
        #_(float-array
           (eduction
          cat
          transform))

        uni (wgpu/create-buffer ctx {:usage #{:Uniform
                                              :CopyDst}
                                     :type :f32
                                     :length 16})

        _ (wgpu/copy-to-buffer ctx uni uni-matrix)

        uni2-matrix
        (dtype/make-container :native-heap
                              :float32
                              (into []
                                    cat
                                    (mat/transpose
                                     light-mat)))

        #_(float-array
         (eduction
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
                             uni
                             buf1
                             normal-buf
                             texture
                             sampler
                             uni2
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
  (let [mesh (mf/import-mesh "/Users/adrian/Downloads/36ee14ff-06a6-47e1-9601-cbd3daf2e491.glb")]
    
    (let [native-buf (draw-mesh mesh [45 45 45  ])
          [w h] [640 640]
          buf (byte-array (native-buffer/native-buffer-byte-len native-buf))
          _ (dtype/copy! native-buf buf)
          pm (skia/pixmap 0 buf w h  skia/kRGBA_8888_SkColorType skia/kUnpremul_SkAlphaType (* w 4))]
      (skia/save-image "doesitwork2.png" pm))
    (prn "done")))


