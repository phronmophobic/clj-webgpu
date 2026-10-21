(ns com.phronemophobic.clj-webgpu.compute
  (:require 
   [membrane.skia :as skia]
   [com.phronemophobic.clj-webgpu.impl.raw :as raw]
   [tech.v3.datatype.native-buffer :as native-buffer]
   [com.phronemophobic.clong.gen.jna.insn :as gen.jna.insn]
   [tech.v3.datatype.ffi :as dt-ffi]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.struct :as dt-struct]
   [membrane.ui :as ui]
   tech.v3.resource
   [clojure.java.io :as io])
  (:import
   (tech.v3.datatype.ffi Pointer)
   com.phronemophobic.membrane.Skia
   java.io.PushbackReader
   java.lang.ref.Cleaner
   java.util.Map)
  (:gen-class))




(defonce ^:private cleaner
  (delay (Cleaner/create)))

(defn ^:private clean-ptr
  ([^Pointer ptr f]
   (clean-ptr ptr f nil))
  ([^Pointer ptr f refs]
   (let [address (.address ptr)
         refs* (volatile! (set refs))]
     (tech.v3.resource/track ptr
                             {:track-type :gc
                              :dispose-fn
                              (fn []
                                
                                (f (Pointer/new address))
                                (vreset! refs* nil))})
     ptr)) )


#_(defn ^:private deep-merge
  ([] {})
  ([m]
   m)
  ([m & ms]
   (apply merge-with deep-merge m ms)))

(defn ^:private convert-kws [m kw-f-pairs]
  (reduce
   (fn [m [kw f]]
     (cond

       (vector? kw)
       (if (get-in m kw)
         (update-in m kw f)
         m)

       (kw m)
       (update m kw f)

       :else 
       m))
   m
   kw-f-pairs))


(comment
  (kw->color-write-mask :All)
  ,)

(defn ^:private camel-to-kebab-case
  "Converts camelCase string to kebab-case."
  [s]
  (->> (re-seq #"[^A-Z]+|[A-Z][^A-Z]*" s)
       (map #(clojure.string/lower-case %))
       (clojure.string/join "-")))


(defmacro defenums []
  
  (let [enums (->> (concat (:enums raw/api)
                           (->> (:flags raw/api)
                                (map (fn [{:keys [flag] :as m}]
                                       (assoc m :enum flag)))))
                   (group-by :enum)
                   (map (fn [[enum-group-name enum-vals]]
                          [enum-group-name (into {}
                                                 (comp
                                                  (map (fn [{:keys [name] :as m}]
                                                         (assoc m :short-name (subs name (inc (count enum-group-name))) )))
                                                  (map (fn [{:keys [short-name value]}]
                                                         [(keyword short-name) value])))

                                                 enum-vals)])))]
    ;; shape is {enum-group-name {enum-kw enum-val}}

    `(do
      ~@(eduction
         (map (fn [[full-enum-name enum-vals]]
                (let [enum-group-name (subs full-enum-name 4)
                      private-name (symbol (str "kw->" (camel-to-kebab-case enum-group-name)))]
                  
                  `(defn ~(with-meta private-name {:private true})
                     [~'kw]
                     (if-let [v# (get ~enum-vals ~'kw)]
                       v#
                       (throw (ex-info "Unknown enum"
                                       {:kw ~'kw
                                        :enum ~enum-group-name})))))))
         enums))))

(defenums)


;; https://developer.mozilla.org/en-US/docs/Web/API/GPUBuffer/usage
;; :usage
(def ^:private
  usage-flags
  {:CopyDst raw/WGPUBufferUsage_CopyDst
   :CopySrc  raw/WGPUBufferUsage_CopySrc 
   ;; :Force32 raw/WGPUBufferUsage_Force32
   :Index  raw/WGPUBufferUsage_Index 
   :Indirect raw/WGPUBufferUsage_Indirect
   :MapRead  raw/WGPUBufferUsage_MapRead 
   :MapWrite raw/WGPUBufferUsage_MapWrite
   :None  raw/WGPUBufferUsage_None 
   :QueryResolve raw/WGPUBufferUsage_QueryResolve
   :Storage  raw/WGPUBufferUsage_Storage 
   :Uniform raw/WGPUBufferUsage_Uniform
   :Vertex  raw/WGPUBufferUsage_Vertex})

(def buffer-types
  #{:i32
    :u32
    :f32
    :f16})

(def ^:private buffer-type-sizes
  {:i32 4
   :u32 4
   :f32 4
   :f16 2})


(defn ^:private ->memory [o]
  (case (.getCanonicalName ^Class (type o))
    "java.lang.String" 
    (dt-ffi/string->c o)

    "byte[]" 
    (dtype/copy!
     o
     (native-buffer/malloc (alength o)))))



(defn ->wgpu-string-view [s]
  (let [data (dt-ffi/string->c s)]
    (tech.v3.resource/track
     (raw/map->WGPUStringView
      {:data (.address (dt-ffi/->pointer data))
       :length (dec (native-buffer/native-buffer-byte-len data))})
     {:dispose-fn (constantly data)})))

(comment
  (require '[clj-java-decompiler.core :refer [decompile disassemble]])
 (clj-java-decompiler.core/decompile-form
  {:decompiler :bytecode}
  (dt-ffi/define-foreign-interface 
                    :void
                    [:int32
                     :pointer
                     '(by-value :WGPUStringView)
                     :pointer
                     :pointer]))
  ,)


(defonce ^:private ctx*
  (delay
    (let [instance (raw/wgpuCreateInstance nil)

          _ (clean-ptr instance
                       raw/wgpuInstanceRelease)

          adapter* (promise)
          _ (raw/wgpuInstanceRequestAdapter instance
                                            nil
                                            (raw/map->WGPURequestAdapterCallbackInfo
                                             {:callback 
                                              (raw/callback-maker
                                               :void
                                               [:int32
                                                :pointer
                                                (by-value :WGPUStringView)
                                                :pointer
                                                :pointer]
                                               (fn [status adapter message user1 user2]
                                                 (deliver adapter* 
                                                          (Pointer. 
                                                           (com.sun.jna.Pointer/nativeValue adapter)))))})
                                            )

          _ (clean-ptr @adapter*
                       raw/wgpuAdapterRelease)

          device* (promise)
          _ (raw/wgpuAdapterRequestDevice @adapter*
                                          nil
                                          (raw/map->WGPURequestDeviceCallbackInfo
                                           {:callback
                                            (raw/callback-maker
                                             :void
                                             [:int32
                                              :pointer
                                              (by-value :WGPUStringView)
                                              :pointer :pointer]
                                             (fn [status device message user1 user2]
                                               (deliver device* 
                                                        (Pointer. 
                                                           (com.sun.jna.Pointer/nativeValue device)))
                                               ))}))
          _ (clean-ptr @device*
                       raw/wgpuDeviceRelease
                       [instance])
          queue (raw/wgpuDeviceGetQueue @device*)
          _ (clean-ptr queue
                       raw/wgpuQueueRelease
                       [@device*])
          ]
      {:instance instance
       :adapter @adapter*
       :device @device*
       :queue queue})))

(comment
  @ctx*
  ,)

(defn create-context []
  ;; Recreating ctxs seems to eventually crash,
  ;; even with proper releasing.
  @ctx*)

(defn create-buffer [ctx {:keys [label usage length type mapped-at-creation?] :as m}]
  (let [;; size is in bytes
        size (* length (get buffer-type-sizes type))
        descriptor (raw/map->WGPUBufferDescriptor
                    {:size size})
        
        descriptor (cond-> descriptor
                     mapped-at-creation? (doto (Map/.put :mappedAtCreation (int 1)))
                     usage (doto (Map/.put :usage (int (reduce
                                                 (fn [flags kw]
                                                   (bit-or flags (get usage-flags kw)))
                                                 0
                                                 usage))))
                     label (doto (Map/.put :label (->wgpu-string-view label))))
        buffer (raw/wgpuDeviceCreateBuffer (:device ctx) descriptor)]
    (clean-ptr buffer
               raw/wgpuBufferRelease)
    (merge
     m
     {:buffer buffer
      :size size})))

(defn membrane-as-native-buffer
  "Loads an image (eg. png) and returns a native buffer in rgba8888 format."
  [view]
  (let [[w h] (ui/bounds view)
        n-bytes (* 4 w h)
        buf (native-buffer/malloc n-bytes)
        skia-resource
        (com.sun.jna.Pointer.
         (.address
          (raw/skia_direct_buffer buf
                                  skia/kRGBA_8888_SkColorType
                                  w h (* 4 w))))]
    
    (try
      (binding [skia/*skia-resource* skia-resource
                skia/*already-drawing* true]
        (skia/draw view))
      buf
      (finally
        (Skia/skia_cleanup skia-resource )))))

(defn load-texture [{:keys [device queue] :as ctx} view]
  (let [
        [width height] (ui/bounds view)
        buf (membrane-as-native-buffer view)
        texture-size (raw/map->WGPUExtent3D
                      {:width width
                       :height height
                       :depthOrArrayLayers 1})
        textureDesc (raw/map->WGPUTextureDescriptor
                     {:dimension raw/WGPUTextureDimension_2D
                      :size texture-size
                      :format raw/WGPUTextureFormat_RGBA8Unorm
                      :mipLevelCount 1
                      :sampleCount 1
                      :usage (bit-or raw/WGPUTextureUsage_TextureBinding raw/WGPUTextureUsage_CopyDst)})
        texture (raw/wgpuDeviceCreateTexture device textureDesc)
        _ (clean-ptr texture
                     raw/wgpuTextureRelease)

        destination (raw/map->WGPUTexelCopyTextureInfo
                     {:texture (.address texture)
                      :mipLevel 0
                      :origin (raw/map->WGPUOrigin3D
                               {:x 0
                                :y 0
                                :z 0})
                      :aspect raw/WGPUTextureAspect_All})

        data-layout (raw/map->WGPUTexelCopyBufferLayout
                     {:offset 0
                      :bytesPerRow (* 4 width)
                      :rowsPerImage height})

        texture-size* (raw/map->WGPUExtent3D
                       {:width width
                        :height height
                        :depthOrArrayLayers 1})
        _ (raw/wgpuQueueWriteTexture queue 
                                     destination
                                     (dt-ffi/->pointer buf)
                                     (native-buffer/native-buffer-byte-len buf)
                                     data-layout
                                     texture-size*)

        texture-view (raw/wgpuTextureCreateView
                      texture
                      (raw/map->WGPUTextureViewDescriptor
                       {:aspect raw/WGPUTextureAspect_All
                        :baseArrayLayer 0
                        :arrayLayerCount 1
                        :baseMipLevel 0
                        :mipLevelCount 1
                        :dimension raw/WGPUTextureViewDimension_2D
                        :format (:format textureDesc)}))
        _ (clean-ptr texture-view
                     raw/wgpuTextureViewRelease)]
    {:texture texture
     :buf buf
     :texture-view texture-view}))

(comment
  (load-texture (create-context)
                (ui/label "hello world!"))
  
  ,)

(defn create-sampler [ctx]
  (let [sampler (raw/wgpuDeviceCreateSampler
                 (:device ctx)
                 (raw/map->WGPUSamplerDescriptor
                  {
                   :addressModeU raw/WGPUAddressMode_Repeat
                   :addressModeV raw/WGPUAddressMode_Repeat
                   :addressModeW raw/WGPUAddressMode_Repeat
                   :magFilter raw/WGPUFilterMode_Linear
                   :minFilter raw/WGPUFilterMode_Linear
                   :mipmapFilter raw/WGPUMipmapFilterMode_Linear
                   :lodMinClamp 0.0
                   :lodMaxClamp 8.0
                   :compare raw/WGPUCompareFunction_Undefined
                   :maxAnisotropy 1}))]
    (clean-ptr sampler
               raw/wgpuSamplerRelease)
    {:sampler sampler}))



(defn create-shader [ctx {:keys [src label]}]
  (let [chain 
        (raw/map->WGPUChainedStruct
         {:sType raw/WGPUSType_ShaderSourceWGSL})
        

        next-in-chain
        (raw/map->WGPUShaderSourceWGSL
         {:chain chain
          :code (->wgpu-string-view src)})
        #_(doto (WGPUShaderModuleWGSLDescriptorByReference.)
                        (.writeField "chain" chain)
                        (.writeField "code" (->memory src)))
        


        descriptor
        (raw/map->WGPUShaderModuleDescriptor
         (merge
          {:nextInChain (.address (dt-ffi/->pointer next-in-chain))}
          (when label
            {:label (->wgpu-string-view label)})))

        shader (raw/wgpuDeviceCreateShaderModule (:device ctx)
                                                 descriptor)]
    (assert shader)
    ;; I'm not sure this is required, but the purpose
    ;; is to prevent the unlikely gc of these before the shader is created.
    (identity [chain next-in-chain descriptor])
    (clean-ptr shader raw/wgpuShaderModuleRelease)

    shader))


(def shaderSource
  "
@vertex
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> @builtin(position) vec4<f32> {
	var p = vec2f(0.0, 0.0);
	if (in_vertex_index == 0u) {
		p = vec2f(-0.5, -0.5);
	} else if (in_vertex_index == 1u) {
		p = vec2f(0.5, -0.5);
	} else {
		p = vec2f(0.0, 0.5);
	}
	return vec4f(p, 0.0, 1.0);
}

@fragment
fn fs_main() -> @location(0) vec4f {
 return vec4f(0.0, 0.4, 1.0, 1.0);
}
")


(comment
  (create-shader (create-context) 
                 {:src shaderSource
                  :label""})
  ,)



(defn copy-to-buffer [ctx buffer data]
  (raw/wgpuQueueWriteBuffer (:queue ctx) (:buffer buffer) 
                              0
                              data
                              (native-buffer/native-buffer-byte-len data)))

(defn create-compute-pipeline [ctx {:keys [entry-point shader]}]
  (raw/with-refs
   (let [entry-point (or entry-point "main") 
         
         compute-pipeline-descriptor
         (raw/map->WGPUComputePipelineDescriptor
          {:label (raw/->wgpu-string-view "compute_pipeline")
           :compute (raw/map->WGPUComputeState
                     {:module (->address shader)
                      :entryPoint (raw/->wgpu-string-view entry-point)})})]
     (tech.v3.resource/track
      (raw/wgpuDeviceCreateComputePipeline
       (:device ctx)
       (-> compute-pipeline-descriptor
           dt-ffi/->pointer))
      {:dispose-fn (fn []
                     (vreset! refs nil  ))}))))

(defn save-texture2
  "Copy texture into native-buffer, `buf`.
  
  If buf is nil, then one will be allocated.
  
  returns buf."
  ([ctx buf texture width height]
   (let [{:keys [device queue]} ctx
         
         refs (volatile! [])
         ref! (fn [o]
                (vswap! refs conj o)
                o)
         ->address (fn [o]
                     (-> o ref! dt-ffi/->pointer dt-ffi/pointer->address)) 

         encoder (raw/wgpuDeviceCreateCommandEncoder
                  device
                  (raw/map->WGPUCommandEncoderDescriptor
                   {:label (->wgpu-string-view "command_encoder")})
                  )
         source (raw/map->WGPUTexelCopyTextureInfo
                 {:texture (->address texture)})
         
         pixelBuffer-size (* 4 width height)
         pixelBuffer (raw/wgpuDeviceCreateBuffer
                      device
                      (raw/map->WGPUBufferDescriptor
                       {:usage (int
                                (bit-or raw/WGPUBufferUsage_MapRead raw/WGPUBufferUsage_CopyDst))
                        :size pixelBuffer-size
                        :mappedAtCreation (int 0)}))
         
         destination (raw/map->WGPUTexelCopyBufferInfo
                      {:buffer (->address pixelBuffer)
                       :layout
                       (raw/map->WGPUTexelCopyBufferLayout
                        {:bytesPerRow (int (* 4 width))
                         :offset 0
                         :rowsPerImage height})})
         _ (raw/wgpuCommandEncoderCopyTextureToBuffer
            encoder
            source
            destination
            (raw/map->WGPUExtent3D
             {:width (int width)
              :height (int height)
              :depthOrArrayLayers (int 1)}))
         
         queue (raw/wgpuDeviceGetQueue device)
         
         command-buffer (raw/wgpuCommandEncoderFinish
                         encoder
                         (raw/map->WGPUCommandBufferDescriptor
                          {:label (->wgpu-string-view "command_buffer")}))
         
         _ (raw/wgpuQueueSubmit queue 1 
                                (dt-ffi/make-ptr :pointer (->address command-buffer)))
         
         _ (raw/wgpuBufferMapAsync pixelBuffer raw/WGPUMapMode_Read 0 pixelBuffer-size
                                   (raw/map->WGPUBufferMapCallbackInfo
                                    {:callback 
                                     (raw/callback-maker
                                      :void
                                      [:int32
                                       (by-value :WGPUStringView)
                                       :pointer
                                       :pointer]
                                      (fn [status message user1 user2]
                                        #_(prn "buffer status" (raw/wgpu-string-view->string (.toStruct message)))))}))
         
         _ (raw/wgpuDevicePoll device 1  nil)
         bs-ptr (raw/wgpuBufferGetMappedRange pixelBuffer 0 pixelBuffer-size)
         
         buf (or buf
                 (native-buffer/malloc pixelBuffer-size))
         _ (dtype/copy! (native-buffer/wrap-address (->address bs-ptr) pixelBuffer-size)
                        buf)]
     (raw/wgpuCommandEncoderRelease encoder)
     (raw/wgpuCommandBufferRelease command-buffer)
     (raw/wgpuBufferRelease pixelBuffer)
     
     (vreset! refs nil)

     buf)))

(defn copy-from-buffer
  "Copies data from gpu to dest native buffer. If `dest` is nil, return a new buffer."
  ([ctx buffer]
   (copy-from-buffer ctx buffer nil))
  ([ctx buffer dest]
   (let [dest (or dest (native-buffer/malloc (:size buffer)))
         result (promise)]
     
     (raw/wgpuBufferMapAsync (:buffer buffer) raw/WGPUMapMode_Read 0 (:size buffer)
                             (raw/map->WGPUBufferMapCallbackInfo
                              {:callback 
                               (raw/callback-maker
                                :void
                                [:int32
                                 (by-value :WGPUStringView)
                                 :pointer
                                 :pointer]
                                (fn [status message user1 user2]
                                  (let [success? (= raw/WGPUMapAsyncStatus_Success status)]
                                    #_(prn "buffer status" :success? success? )
                                    (deliver result
                                             {:success? success?
                                              :message (raw/wgpu-string-view->string (.toStruct message))})
                                    )))}))
     (raw/wgpuDevicePoll (:device ctx) 1 nil)

     (when (not (:success? @result))
       (throw (ex-info (:message @result)
                       {:ctx ctx
                        :buffer buffer
                        :dest dest})))
     (let [ptr (raw/wgpuBufferGetMappedRange (:buffer buffer) 0 (:size buffer))]
       (dtype/copy! (native-buffer/wrap-address (dt-ffi/pointer->address ptr) (:size buffer))
                    (-> dest
                        (native-buffer/set-native-datatype :int8)))

       dest))))

(defn create-texture
    "Creates a texture. Also automatically creates a matching texture view using the same properties as the underlying texture.

  The following options are available
  :label A name for the texture. Can be useful for debugging.
  :view-label A name for the texture view. Can be useful for debugging.
  :size A mape like {:width width :height height :depthOrArraylayers 1}
  :mipLevelCount default 1
  :sampleCount default 1
  :dimension one of #{:d1 :d2 :d3 :force32}. default :d2
  :format one of `texture-formats`. default :RGBA8Unorm
  :usage A subset of #{:CopyDst :Force32 :StorageBinding :None :TextureBinding :CopySrc :RenderAttachment}.
  :aspect One of #{:Force32 :All :StencilOnly :DepthOnly}
  
"
    [ctx texture-descriptor]
  (let [texture-descriptor
        (convert-kws
         texture-descriptor
         [[:label ->wgpu-string-view]
          [:size raw/map->WGPUExtent3D]
          [:dimension kw->texture-dimension]
          [:format kw->texture-format]
          [:usage #(reduce
                    (fn [i kw]
                      (bit-or i (kw->texture-usage kw)))
                    0
                    %)]])

        texture (raw/wgpuDeviceCreateTexture
                 (:device ctx)
                 (raw/map->WGPUTextureDescriptor texture-descriptor))]
    (clean-ptr texture raw/wgpuTextureRelease)
    texture))


(defn create-texture-view [texture texture-view-descriptor]
  (let [texture-view-descriptor
        (convert-kws
         texture-view-descriptor
         [[:label ->wgpu-string-view]
          [:aspect kw->texture-aspect]
          [:format kw->texture-format]
          [:dimension kw->texture-dimension]])

        texture-view (raw/wgpuTextureCreateView
                      texture
                      (raw/map->WGPUTextureViewDescriptor
                       texture-view-descriptor))
        ]
    (clean-ptr texture-view raw/wgpuTextureViewRelease [texture])
    texture-view))

(defn poll [device wait?]
  (raw/wgpuDevicePoll device (if wait? 1 0)  nil))


(defn create-bind-group [device bind-group-layout bindings]
  (let [->address #(-> % dt-ffi/->pointer dt-ffi/pointer->address)
        entries (raw/struct-array
                 :WGPUBindGroupEntry
                 (into []
                       (map-indexed
                        (fn [i binding]
                          (cond
                            (:buffer binding)
                            {:binding i
                             :buffer (->address (:buffer binding))
                             :offset 0
                             :size (:size binding)}

                            (:texture-view binding)
                            {:binding i
                             :textureView (->address (:texture-view binding))}

                            (:sampler binding)
                            {:binding i
                             :sampler (->address (:sampler binding))}

                            :else (throw (ex-info "Unknown buffer type"
                                                  {:binding binding})))))
                       bindings))
        

        bind-group (raw/wgpuDeviceCreateBindGroup
                    device
                    (raw/map->WGPUBindGroupDescriptor
                     {:layout (->address bind-group-layout)
                      :entryCount (count bindings)
                      :entries (-> entries dt-ffi/->pointer dt-ffi/pointer->address)}
                     ))]
    (identity entries
               )
    (clean-ptr bind-group raw/wgpuBindGroupRelease)
    bind-group))



(defn ^:private encode-copy-pass [ctx command-encoder pass]
  (let [{:keys [source destination]} pass
        source-offset (or (:source-offset pass) 0)
        destination-offset (or (:destination-offset pass) 0)
        size (or (:size pass) (:size source))]
    (raw/wgpuCommandEncoderCopyBufferToBuffer command-encoder
                                              (:buffer source) source-offset
                                              (:buffer destination) destination-offset
                                              size)))

(defn ^:private encode-compute-pass [ctx command-encoder pass]
  (raw/with-refs
   (let [{:keys [label pipeline bind-groups dispatch-work-groups]} pass
         
         
         compute-pass-encoder (raw/wgpuCommandEncoderBeginComputePass
                               command-encoder
                               (-> (raw/map->WGPUComputePassDescriptor
                                    {:label (raw/->wgpu-string-view (or label "compute_pass"))})
                                   ref!
                                   dt-ffi/->pointer))
         _ (raw/wgpuComputePassEncoderSetPipeline compute-pass-encoder pipeline)
         
         _ (doseq [[group-index bind-group-info] (map-indexed vector bind-groups)]
             (let [bind-group-layout (raw/wgpuComputePipelineGetBindGroupLayout pipeline group-index)
                   bind-group (create-bind-group (:device ctx) bind-group-layout bind-group-info)]
               (raw/wgpuComputePassEncoderSetBindGroup compute-pass-encoder group-index bind-group 0 nil)))
         
         _ (raw/wgpuComputePassEncoderDispatchWorkgroups compute-pass-encoder
                                                         (:x dispatch-work-groups 1)
                                                         (:y dispatch-work-groups 1)
                                                         (:z dispatch-work-groups 1))
         
         _ (raw/wgpuComputePassEncoderEnd compute-pass-encoder)]
     (raw/wgpuComputePassEncoderRelease compute-pass-encoder)
     (vreset! refs nil)
     nil)))

(defn ^:private encode-render-pass [ctx command-encoder pass]
  (let [draws (:draws pass)
        pass (:render-pass pass)

        refs (volatile! [])
        ref! (fn [o]
               (vswap! refs conj o)
               o)

        ->pointer (fn [o]
                    (ref!
                     (-> o
                         dt-ffi/->pointer
                         dt-ffi/pointer->address)))

        render-pass (raw/wgpuCommandEncoderBeginRenderPass
                     command-encoder
                     (raw/map->WGPURenderPassDescriptor
                      (merge
                       pass
                       {:colorAttachmentCount (count (:colorAttachments pass))
                        :colorAttachments
                        (->pointer 
                         (raw/struct-array
                          :WGPURenderPassColorAttachment
                          (into []
                                (map (fn [color-attachment]
                                       (convert-kws
                                        color-attachment
                                        [[:loadOp kw->load-op]
                                         [:storeOp kw->store-op]
                                         [:view ->pointer]
                                         [:clearValue raw/map->WGPUColor]])))
                                (:colorAttachments pass))))
                        :depthStencilAttachment
                        (->pointer
                         (raw/map->WGPURenderPassDepthStencilAttachment
                          (convert-kws
                           (:depthStencilAttachment pass)
                           [[:depthLoadOp kw->load-op]
                            [:view ->pointer]
                            [:depthStoreOp kw->store-op]])))})))]
    (doseq [draw draws]
      (let [pipeline (:pipeline draw)
            group 0]
        (raw/wgpuRenderPassEncoderSetPipeline render-pass pipeline)
        (when (seq (:bindings draw))
          (let [bind-group-layout (raw/wgpuRenderPipelineGetBindGroupLayout pipeline group)
                bind-group (create-bind-group (:device ctx) bind-group-layout (:bindings draw))]
            (raw/wgpuRenderPassEncoderSetBindGroup render-pass group bind-group 0 nil)))
        (raw/wgpuRenderPassEncoderDraw  render-pass (:vertex-count draw) (:instance-count draw 1) (:first-instance draw 0) (:first-vertex draw 0))))
    (raw/wgpuRenderPassEncoderEnd render-pass)
    (raw/wgpuRenderPassEncoderRelease render-pass)
    (vreset! refs nil)
    nil))

(defn submit [ctx passes]

  (let [group 0
        device (:device ctx)
        queue (:queue ctx)

        command-encoder (raw/wgpuDeviceCreateCommandEncoder
                         device
                         (raw/map->WGPUCommandEncoderDescriptor
                          {}))

        _ (doseq [pass passes]
            (cond
              ;; this needs to get cleaned up
              ;; there are still other types of passes
              (:render-pass pass)
              (encode-render-pass ctx command-encoder pass)

              (:compute-pass pass)
              (encode-compute-pass ctx command-encoder pass)
              
              (:copy-pass pass)
              (encode-copy-pass ctx command-encoder pass)

              :else (throw (ex-info "Unrecognized pass"
                                    {:pass pass}))))

        command-buffer (raw/wgpuCommandEncoderFinish
                        command-encoder
                        (raw/map->WGPUCommandBufferDescriptor
                         {}))


        

        _ (raw/wgpuQueueSubmit queue 1 
                               (dt-ffi/make-ptr :pointer 
                                                (-> command-buffer
                                                    dt-ffi/pointer->address)))

        _ (raw/wgpuCommandEncoderRelease command-encoder)
        _ (raw/wgpuCommandBufferRelease command-buffer)]
    nil))







(defn create-render-pipeline [ctx pipeline-descriptor]
  (let [
        refs (volatile! [] )
        ref! (fn [o] 
               (vswap! refs conj o)
               o)
        struct->pointer (fn [s]
                          (-> s
                              ref!
                              dt-ffi/->pointer
                              dt-ffi/pointer->address))

        ;; convert strings to Memory for some key paths
        pipeline-descriptor
        (convert-kws
         pipeline-descriptor
         [[[:vertex :entryPoint] ->wgpu-string-view]
          [[:vertex :module] struct->pointer]
          [[:fragment :entryPoint] ->wgpu-string-view]
          [[:fragment :module] struct->pointer]])

        ->stencil (fn [m]
                    (raw/map->WGPUStencilFaceState
                     (convert-kws m
                                  [[:compare kw->compare-function]
                                   [:failOp kw->stencil-operation]
                                   [:depthFailOp kw->stencil-operation]
                                   [:passOp kw->stencil-operation]])))

        ->blend-component
        (fn [m]
          (raw/map->WGPUBlendComponent
           (convert-kws m
                        [[:srcFactor kw->blend-factor ]
                         [:dstFactor kw->blend-factor]
                         [:operation kw->blend-operation]])))

        ->blend-state
        (fn [m]
          (struct->pointer
           (raw/map->WGPUBlendState
                         (convert-kws m
                                      [[:color ->blend-component]
                                       [:alpha ->blend-component]]))))

        

        pipeline-descriptor
        (update pipeline-descriptor
                :fragment
                (fn [fragment]
                  (let [targets (ref!
                                 (raw/struct-array
                                  :WGPUColorTargetState
                                  (into []
                                        (map (fn [target]
                                               (convert-kws target
                                                            [[:format kw->texture-format]
                                                             [:blend ->blend-state]
                                                             [:writeMask kw->color-write-mask]])))
                                        (:targets fragment))))]
                    (merge
                     fragment
                     {:targets (-> targets dt-ffi/->pointer dt-ffi/pointer->address)
                      :targetCount (count (:targets fragment))}))))

        descriptor*
        (raw/map->WGPURenderPipelineDescriptor
         {:vertex (raw/map->WGPUVertexState
                   (:vertex pipeline-descriptor))
          :primitive
          (raw/map->WGPUPrimitiveState
           (convert-kws
            (:primitive pipeline-descriptor)
            [[:topology kw->primitive-topology]
             [:cullMode kw->cull-mode]]))
          :depthStencil
          (struct->pointer
           (raw/map->WGPUDepthStencilState
            (convert-kws
             (:depthStencil pipeline-descriptor)
             [[:depthCompare kw->compare-function]
              [:format kw->texture-format]
              [:stencilBack ->stencil]
              [:stencilFront ->stencil]])))
          :fragment
          (struct->pointer (raw/map->WGPUFragmentState
           (:fragment pipeline-descriptor)))
          :multisample
          (raw/map->WGPUMultisampleState
           (:multisample pipeline-descriptor))
          ,})
        pipeline (raw/wgpuDeviceCreateRenderPipeline (:device ctx) descriptor*)

        ;; hang onto shader modules
        _ (ref! [(get-in pipeline-descriptor [:vertex :module])
                 (get-in pipeline-descriptor [:fragment :module])])]
    (assert pipeline)
    (clean-ptr pipeline
               (fn [p]
                 (raw/wgpuRenderPipelineRelease p)
                 (vreset! refs nil)))
    pipeline))

(defn -main [& args]
  
  ;; (prn (create-shader (create-context)
  ;;                     {:src shaderSource}))
  ;; (prn (create-shader (create-context)
  ;;                     {:src ""}))
  
  #_(prn (load-texture
          (create-context)
          (ui/filled-rectangle [1 0 0] 10 10)))
  
  #_(prn (create-buffer (create-context)
                        {:length 10
                         :type :i32
                         :usage #{:MapRead :CopyDst}})))