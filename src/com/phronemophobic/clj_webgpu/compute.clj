(ns com.phronemophobic.clj-webgpu.compute
  (:require 
   [com.phronemophobic.clj-webgpu.impl.raw :as raw]
   [clojure.java.io :as io])
  (:import
   java.io.PushbackReader
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.LongByReference
   com.sun.jna.ptr.IntByReference
   com.sun.jna.ptr.ByteByReference
   com.sun.jna.Structure
   java.awt.image.BufferedImage
   javax.imageio.ImageIO
   java.lang.ref.Cleaner
   (com.phronemophobic.clj_webgpu.impl.raw.structs
    WGPUBindGroupLayoutEntryByReference
    WGPUShaderModuleDescriptorByReference
    WGPUShaderModuleWGSLDescriptorByReference
    WGPUBufferDescriptorByReference
    WGPUComputePipelineDescriptorByReference
    WGPUChainedStruct
    WGPUProgrammableStageDescriptor
    WGPUBindGroupDescriptorByReference
    WGPUBindGroupEntryByReference
    WGPUCommandEncoderDescriptorByReference
    WGPUComputePassDescriptorByReference
    WGPUCommandBufferDescriptorByReference))
  (:gen-class))


(defonce ^:private cleaner
  (delay (Cleaner/create)))

(defn ^:private clean-ptr
  ([ptr f]
   (clean-ptr ptr f nil))
  ([ptr f refs]
   (let [raw-ptr (Pointer/nativeValue ptr)
         refs* (volatile! (set refs))]
     (.register ^Cleaner @cleaner ptr
                (fn []
                  (f raw-ptr)
                  (vreset! refs* nil)))
     nil)) )

(defn ^:private write-field [^Structure o field val]
  (.writeField o field val)
  o)


(defn ^:private deep-merge
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

(defmacro defenum [var-name private-name enum]
  `(do
     (let [soffset# (inc (count ~enum))
           m# (into {}
                    (comp
                     (keep (fn [{:keys [~'enum ~'name ~'value]}]
                             (when (= ~'enum ~enum)
                               [(keyword (subs ~'name soffset#))
                                ~'value]))))
                    (:enums raw/api))]
      (defn ~(with-meta private-name {:private true})
        [kw#]
        (if-let [v# (get m# kw#)]
          v#
          (throw (ex-info "Unknown enum"
                          {:kw kw#}))
          ))
      (def ~var-name
        ~(str "Possible " var-name)
        (into #{} (keys m#)))))
  )

(defn ^:private camel-to-kebab-case
  "Converts camelCase string to kebab-case."
  [s]
  (->> (re-seq #"[^A-Z]+|[A-Z][^A-Z]*" s)
       (map #(clojure.string/lower-case %))
       (clojure.string/join "-")))


(defmacro defenums []
  `(do
     ~@(eduction

        (map (fn [enum]
               (let [enum-name (subs (:enum enum) 4)
                     kb (camel-to-kebab-case enum-name)]
                `(defenum ~(symbol (str kb "s")) ~(symbol (str "kw->" kb)) ~(:enum enum)))))
        (:enums raw/api))))
(defenums)


;; https://developer.mozilla.org/en-US/docs/Web/API/GPUBuffer/usage
;; :usage
(def ^:private
  usage-flags
  {:CopyDst raw/WGPUBufferUsage_CopyDst
   :CopySrc  raw/WGPUBufferUsage_CopySrc 
   :Force32 raw/WGPUBufferUsage_Force32
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
    "java.lang.String" (let [^String o o
                             buf (.getBytes o "utf-8")
                             len (alength buf)]
                         (doto (Memory. (inc len))
                           (.write 0 buf 0 len)
                           (.setByte len 0)))

    "byte[]" (let [^bytes o o]
               (doto (Memory. (alength o))
                 (.write 0 o 0 (alength o))))))


(defonce ^:private
  ctx*
  (delay
    (let [instance (raw/wgpuCreateInstance nil)
          _ (clean-ptr instance
                       raw/wgpuInstanceRelease)
          adapter* (promise)
          _ (raw/wgpuInstanceRequestAdapter instance
                                            nil
                                            (fn [status adapter message user-date]
                                              (deliver adapter* adapter))
                                            nil)
          _ (clean-ptr @adapter*
                       raw/wgpuAdapterRelease)

          supported-limits (raw/map->WGPUSupportedLimits* {} )
          _ (raw/wgpuAdapterGetLimits @adapter*
                                      supported-limits)

          device* (promise)
          _ (raw/wgpuAdapterRequestDevice @adapter*
                                          nil
                                          #_(raw/map->WGPUDeviceDescriptor*
                                             {:requiredLimits
                                              (raw/map->WGPURequiredLimits*
                                               {:limits 
                                                (raw/map->WGPULimits
                                                 (assoc (into {} (:limits supported-limits))
                                                        :maxTextureDimension2D 1000))})})
                                          (fn [status device message user-data]
                                            (deliver device* device))
                                          nil)
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

(defn create-context []
  ;; Recreating ctxs seems to eventually crash,
  ;; even with proper releasing.
  @ctx*)

(defn create-buffer [ctx {:keys [label usage length type mapped-at-creation?] :as m}]
  (let [size (* length (get buffer-type-sizes type))
        descriptor (doto (WGPUBufferDescriptorByReference.)
                     (.writeField "size" size))
        
        descriptor (cond-> descriptor
                     mapped-at-creation? (write-field "mappedAtCreation" (int 1))
                     usage (write-field "usage"
                                        (int (reduce
                                              (fn [flags kw]
                                                (bit-or flags (get usage-flags kw)))
                                              0
                                              usage)))
                     label (write-field "label" (->memory label)))
        buffer (raw/wgpuDeviceCreateBuffer (:device ctx) descriptor)]
    (clean-ptr buffer
               raw/wgpuBufferRelease)
    (merge
     m
     {:buffer buffer
      :size size})))

(defn load-image
  "`src` can be any value supported by clojure.java.io/input-stream."
  ^BufferedImage [src]
  (with-open [is (clojure.java.io/input-stream src)]
    (let [image-stream (ImageIO/createImageInputStream is)
          buffered-image (ImageIO/read image-stream)]
      buffered-image)))

(defn load-texture [{:keys [device queue] :as ctx} src]
  ;;   int width, height, channels;
  ;; unsigned char *pixelData = stbi_load(path.string().c_str(), &width, &height, &channels, 4 /* force 4 channels */);
  ;; if (nullptr == pixelData) return nullptr;

  ;; WGPUTextureDescriptor textureDesc = {};
  ;; textureDesc.nextInChain = nullptr;
  ;; textureDesc.dimension = WGPUTextureDimension_2D;
  ;; textureDesc.format = WGPUTextureFormat_RGBA8Unorm; // by convention for bmp, png and jpg file. Be careful with other formats.
  ;; textureDesc.mipLevelCount = 1;
  ;; textureDesc.sampleCount = 1;
  ;; textureDesc.size = { (unsigned int)width, (unsigned int)height, 1 };
  ;; textureDesc.usage = WGPUTextureUsage_TextureBinding | WGPUTextureUsage_CopyDst;
  ;; textureDesc.viewFormatCount = 0;
  ;; textureDesc.viewFormats = nullptr;
  ;; WGPUTexture texture = wgpuDeviceCreateTexture(device, &textureDesc);

  (let [ ;; get pixel data
        img (load-image src)
        pixel-data (-> img
                       (.getData)
                       ^java.awt.image.DataBufferByte
                       (.getDataBuffer)
                       (.getData))
        width (.getWidth img)
        height (.getHeight img)

        ;; still needs help from shader
        ;; to convert to raw/WGPUTextureFormat_RGBA8Unorm
        _ (assert (= BufferedImage/TYPE_4BYTE_ABGR (.getType img)))
        texture-size (raw/map->WGPUExtent3D
                      {:width width
                       :height height
                       :depthOrArrayLayers 1})
        textureDesc (raw/map->WGPUTextureDescriptor*
                     {:dimension raw/WGPUTextureDimension_2D
                      :size texture-size
                      :format raw/WGPUTextureFormat_RGBA8Unorm
                      :mipLevelCount 1
                      :sampleCount 1
                      :usage (bit-or raw/WGPUTextureUsage_TextureBinding raw/WGPUTextureUsage_CopyDst)})
        texture (raw/wgpuDeviceCreateTexture device textureDesc)
        _ (clean-ptr texture
                     raw/wgpuTextureRelease)

        ;; // Auxiliary function for loadTexture
        ;; static void writeMipMaps(
        ;;     WGPUDevice device,
        ;;     WGPUTexture texture,
        ;;     WGPUExtent3D textureSize,
        ;;     [[maybe_unused]] uint32_t mipLevelCount, // not used yet
        ;;     const unsigned char* pixelData)
        ;; {
        ;;     WGPUImageCopyTexture destination = {};
        ;;     destination.texture = texture;
        ;;     destination.mipLevel = 0;
        ;;     destination.origin = { 0, 0, 0 };
        ;;     destination.aspect = WGPUTextureAspect_All;

        ;;     WGPUTextureDataLayout source = {};
        ;;     source.offset = 0;
        ;;     source.bytesPerRow = 4 * textureSize.width;
        ;;     source.rowsPerImage = textureSize.height;

        ;;     WGPUQueue queue = wgpuDeviceGetQueue(device);
        ;;     wgpuQueueWriteTexture(queue, destination, pixelData, 4 * textureSize.width * textureSize.height, source, textureSize);
        ;;     wgpuQueueRelease(queue);
        ;; }
        destination (raw/map->WGPUImageCopyTexture*
                     {:texture texture
                      :mipLevel 0
                      :origin (raw/map->WGPUOrigin3D
                               {:x 0
                                :y 0
                                :z 0})
                      :aspect raw/WGPUTextureAspect_All})

        source (raw/map->WGPUTextureDataLayout*
                {:offset 0
                 :bytesPerRow (* 4 width)
                 :rowsPerImage height})

        ;; convert from ABGR to RGBA
        _ (doseq [i (range (* width height))
                  :let [idx (* 4 i)
                        a (aget pixel-data (+ idx 0))
                        b (aget pixel-data (+ idx 1))
                        g (aget pixel-data (+ idx 2))
                        r (aget pixel-data (+ idx 3))]]
            (aset pixel-data (+ idx 0) r)
            (aset pixel-data (+ idx 1) g)
            (aset pixel-data (+ idx 2) b)
            (aset pixel-data (+ idx 3) a))

        pixel-buf (doto (Memory. (alength pixel-data))
                    (.write 0 pixel-data 0 (alength pixel-data)))

        texture-size* (raw/map->WGPUExtent3D*
                       {:width width
                        :height height
                        :depthOrArrayLayers 1})
        _ (raw/wgpuQueueWriteTexture queue destination pixel-data (alength pixel-data) source texture-size*)

        ;; TextureViewDescriptor textureViewDesc;
        ;; textureViewDesc.aspect = WGPUTextureAspect_All;
        ;; textureViewDesc.baseArrayLayer = 0;
        ;; textureViewDesc.arrayLayerCount = 1;
        ;; textureViewDesc.baseMipLevel = 0;
        ;; textureViewDesc.mipLevelCount = textureDesc.mipLevelCount;
        ;; textureViewDesc.dimension = WGPUTextureViewDimension_2D;
        ;; textureViewDesc.format = textureDesc.format;
        ;; *pTextureView = wgpuTextureCreateView(texture, &textureViewDesc);
        texture-view (raw/wgpuTextureCreateView
                      texture
                      (raw/map->WGPUTextureViewDescriptor*
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
     :texture-view texture-view}))

(defn create-sampler [ctx]
  (let [
	;; SamplerDescriptor samplerDesc;
	;; samplerDesc.addressModeU = AddressMode::Repeat;
	;; samplerDesc.addressModeV = AddressMode::Repeat;
	;; samplerDesc.addressModeW = AddressMode::Repeat;
	;; samplerDesc.magFilter = FilterMode::Linear;
	;; samplerDesc.minFilter = FilterMode::Linear;
	;; samplerDesc.mipmapFilter = MipmapFilterMode::Linear;
	;; samplerDesc.lodMinClamp = 0.0f;
	;; samplerDesc.lodMaxClamp = 8.0f;
	;; samplerDesc.compare = CompareFunction::Undefined;
	;; samplerDesc.maxAnisotropy = 1;
	;; Sampler sampler = device.createSampler(samplerDesc);
        sampler (raw/wgpuDeviceCreateSampler
                 (:device ctx)
                 (raw/map->WGPUSamplerDescriptor*
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
  (let [chain (doto (WGPUChainedStruct.)
                (.writeField "sType" raw/WGPUSType_ShaderModuleWGSLDescriptor))

        ^WGPUShaderModuleWGSLDescriptorByReference
        next-in-chain (doto (WGPUShaderModuleWGSLDescriptorByReference.)
                        (.writeField "chain" chain)
                        (.writeField "code" (->memory src)))

        descriptor
        (doto (WGPUShaderModuleDescriptorByReference.)
          (.writeField "nextInChain" (.getPointer next-in-chain)))

        descriptor (cond-> descriptor
                     label (write-field "label" (->memory label)))

        shader (raw/wgpuDeviceCreateShaderModule (:device ctx)
                                                 descriptor)]
    (assert shader)
    ;; I'm not sure this is required, but the purpose
    ;; is to prevent the unlikely gc of these before the shader is created.
    (identity [chain descriptor])
    (clean-ptr shader raw/wgpuShaderModuleRelease)

    shader))

(defn copy-to-buffer [ctx buffer data]
  (let [mem (Memory. (:size buffer))]
    (case (:type buffer)
      :i32 (let [^ints data data] (.write mem 0 data 0 (alength data)))
      :u32 (let [^ints data data] (.write mem 0 data 0 (alength data)))
      :f32 (let [^floats data data] (.write mem 0 data 0 (alength data)))
      ;; :f16
      )
    (raw/wgpuQueueWriteBuffer (:queue ctx) (:buffer buffer) 0 mem (.size mem))))

(defn compute [ctx {:keys [entry-point
                           shader
                           group
                           bindings
                           workgroups]}]
  (let [entry-point (or entry-point "main")
        group (int (or group 0))
        device (:device ctx)

        ;; I'm not sure this is required, but the purpose
        ;; is to prevent the unlikely gc of these before the shader is created.
        refs (volatile! #{})
        ref! (fn [o]
               (vswap! refs conj o)
               o)

        compute-pipeline-descriptor
        (ref!
         (doto (WGPUComputePipelineDescriptorByReference.)
           (.writeField "label" (->memory "compute_pipeline"))
           (.writeField "compute"
                        (doto (WGPUProgrammableStageDescriptor.)
                          (.writeField "module" shader)
                          (.writeField "entryPoint" (->memory entry-point))))))

        compute-pipeline (raw/wgpuDeviceCreateComputePipeline
                          device
                          compute-pipeline-descriptor)

        bind-group-layout (raw/wgpuComputePipelineGetBindGroupLayout compute-pipeline group)

        ^objects
        entries-arr (ref!
                     (.toArray (WGPUBindGroupEntryByReference.) (count bindings)))
        _ (doseq [[i buffer] (map-indexed vector bindings)]
            (let [^WGPUBindGroupEntryByReference
                  entry (aget entries-arr i)]
              (doto entry
                (.writeField "binding" (int i))
                (.writeField "buffer" (:buffer buffer))
                (.writeField "offset" 0)
                (.writeField "size" (:size buffer)))))

        bind-group (raw/wgpuDeviceCreateBindGroup
                    device
                    (ref!
                     (doto (WGPUBindGroupDescriptorByReference.)
                       (.writeField "label" (->memory "bind_group"))
                       (.writeField "layout" bind-group-layout)
                       (.writeField "entryCount" (long (alength entries-arr)))
                       (.writeField "entries" (aget entries-arr 0)))))

        command-encoder (raw/wgpuDeviceCreateCommandEncoder
                         device
                         (ref!
                          (doto (WGPUCommandEncoderDescriptorByReference.)
                            (.writeField "label" (->memory "command_encoder")))))

        compute-pass-encoder (raw/wgpuCommandEncoderBeginComputePass
                              command-encoder
                              (ref!
                               (doto (WGPUComputePassDescriptorByReference.)
                                 (.writeField "label" (->memory "compute_pass")))))
        _ (raw/wgpuComputePassEncoderSetPipeline compute-pass-encoder compute-pipeline)

        _ (raw/wgpuComputePassEncoderSetBindGroup compute-pass-encoder group bind-group 0 nil)

        _ (raw/wgpuComputePassEncoderDispatchWorkgroups compute-pass-encoder (:x workgroups 1) (:y workgroups 1) (:z workgroups 1))

        _ (raw/wgpuComputePassEncoderEnd compute-pass-encoder)

        command-buffer (raw/wgpuCommandEncoderFinish
                        command-encoder
                        (ref!
                         (doto (WGPUCommandBufferDescriptorByReference.)
                           (.writeField "label" (->memory "command_buffer")))))

        _ (raw/wgpuQueueSubmit (:queue ctx) 1 (ref!
                                               (doto (PointerByReference.)
                                                 (.setValue command-buffer))))]
    
    (vreset! refs nil)
    nil))


(defmacro with-tile [[tile-bind img] & body]
  `(let [img# ~img
         ~tile-bind (.getWritableTile img# 0 0)]
     (try
       ~@body
       (finally
         (.releaseWritableTile img# 0 0)))))

(defn ->buffered-image
  ([buf width height]
   (->buffered-image (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR) buf width height))
  ([^BufferedImage img ^Memory buf width height]
   (let [;; 4 bytes per pixel
         linesize (* 4 width)

         get-buf (fn [y] (.getByteArray buf (* linesize y) linesize))]

     (with-tile [wraster img]
       (doseq [y (range height)]

         (.setDataElements wraster 0 y width 1
                           (get-buf y))))
     img)))

(defn save-png [bufimg f]
  (with-open [os (clojure.java.io/output-stream f)]
    (ImageIO/write ^BufferedImage bufimg "png" os)))

;; https://github.com/eliemichel/LearnWebGPU-Code/blob/6f5e4cf1297b4f06a1e3659b0ecc1ab581709757/save_image.h#L240
(defn save-texture [ctx texture width height]
  (let [{:keys [device queue]} ctx

        ;; 	auto device = m_device;
        ;; 	auto width = m_width;
        ;; 	auto height = m_height;
        ;; 	auto pixelBuffer = m_pixelBuffer;
        ;; 	auto pixelBufferDesc = m_pixelBufferDesc;
        encoder (raw/wgpuDeviceCreateCommandEncoder
                 device
                 (raw/map->WGPUCommandEncoderDescriptor*
                  {:label (->memory "command_encoder")})
                 )
        ;; 	// Start encoding the commands
        ;; 	CommandEncoder encoder = device.createCommandEncoder(Default);

        source (raw/map->WGPUImageCopyTexture*
                {:texture texture})
        
        pixelBuffer-size (* 4 width height)
        pixelBuffer (raw/wgpuDeviceCreateBuffer
                     device
                     (raw/map->WGPUBufferDescriptor*
                      {:usage (int
                               (bit-or raw/WGPUBufferUsage_MapRead raw/WGPUBufferUsage_CopyDst))
                       :size pixelBuffer-size
                       :mappedAtCreation (int 0)}))
        
	;; BufferDescriptor pixelBufferDesc = Default;
	;; pixelBufferDesc.mappedAtCreation = false;
	;; pixelBufferDesc.usage = BufferUsage::MapRead | BufferUsage::CopyDst;
	;; pixelBufferDesc.size = 4 * width * height;
	;; Buffer pixelBuffer = device.createBuffer(pixelBufferDesc);

        destination (raw/map->WGPUImageCopyBuffer*
                     {:buffer pixelBuffer
                      :layout
                      (raw/map->WGPUTextureDataLayout
                       {:bytesPerRow (int (* 4 width))
                        :offset 0
                        :rowsPerImage height})})
        _ (raw/wgpuCommandEncoderCopyTextureToBuffer
           encoder
           source
           destination
           (raw/map->WGPUExtent3D*
            {:width (int width)
             :height (int height)
             :depthOrArrayLayers (int 1)}))
        ;; 	// Get pixels
        ;; 	ImageCopyTexture source = Default;
        ;; 	source.texture = texture;
        ;; 	ImageCopyBuffer destination = Default;
        ;; 	destination.buffer = pixelBuffer;
        ;; 	destination.layout.bytesPerRow = 4 * width;
        ;; 	destination.layout.offset = 0;
        ;; 	destination.layout.rowsPerImage = height;
        ;; 	encoder.copyTextureToBuffer(source, destination, { width, height, 1 });

        queue (raw/wgpuDeviceGetQueue device)

        command-buffer (raw/wgpuCommandEncoderFinish
                        encoder
                        (raw/map->WGPUCommandBufferDescriptor*
                         {:label (->memory "command_buffer")}))
        
        _ (raw/wgpuQueueSubmit queue 1 (doto (PointerByReference.)
                                         (.setValue command-buffer)))

        _ (raw/wgpuBufferMapAsync pixelBuffer raw/WGPUMapMode_Read 0 pixelBuffer-size
                                  (fn [status user-data]
                                    #_(prn "buffer status" status))
                                  nil)

        _ (raw/wgpuDevicePoll device 1  nil)
        bs (raw/wgpuBufferGetMappedRange pixelBuffer 0 pixelBuffer-size)

        ;; 	// Issue commands
        ;; 	Queue queue = device.getQueue();
        ;; 	CommandBuffer command = encoder.finish(Default);
        ;; 	queue.submit(command);

        ;; 	encoder.release();
        ;; 	command.release();

        ;; 	// Map buffer
        ;; 	std::vector<uint8_t> pixels;
        ;; 	bool done = false;
        ;; 	bool failed = false;
        ;; 	auto callbackHandle = pixelBuffer.mapAsync(MapMode::Read, 0, pixelBufferDesc.size, [&](BufferMapAsyncStatus status) {
        ;; 		if (status != BufferMapAsyncStatus::Success) {
        ;; 			failed = true;
        ;; 			done = true;
        ;; 			return;
        ;; 		}
        ;; 		unsigned char* pixelData = (unsigned char*)pixelBuffer.getConstMappedRange(0, pixelBufferDesc.size);
        ;; 		int bytesPerRow = 4 * width;
        ;; 		int success = stbi_write_png(path.string().c_str(), (int)width, (int)height, 4, pixelData, bytesPerRow);

        ;; 		pixelBuffer.unmap();

        ;; 		failed = success == 0;
        ;; 		done = true;
        ;; 	});

        ;; 	// Wait for mapping
        ;; 	while (!done) {
        ;; #ifdef WEBGPU_BACKEND_WGPU
        ;; 		wgpuQueueSubmit(queue, 0, nullptr);
        ;; #else
        ;; 		device.tick();
        ;; #endif
        ;; 	}

        ;; 	queue.release();
        ]
    (raw/wgpuCommandEncoderRelease encoder)
    (raw/wgpuCommandBufferRelease command-buffer)
    (raw/wgpuBufferRelease pixelBuffer)

    bs))

(def texture-format raw/WGPUTextureFormat_RGBA8UnormSrgb)
(defn render [ctx {:keys [vertex-entry-point
                          fragment-entry-point
                          shader
                          bindings
                          instance-count
                          vertex-count
                          width
                          height]}]
  (let [group 0
        device (:device ctx)
        queue (:queue ctx)

        ;; I'm not sure this is required, but the purpose
        ;; is to prevent the unlikely gc of these before the shader is created.
        refs (volatile! #{})
        ref! (fn [o]
               (vswap! refs conj o)
               o)


        ;; layout
        ;; (raw/wgpuDeviceCreatePipelineLayout
        ;;  device
        ;;  (let [

        ;;        ^objects
        ;;        entries-arr (ref!
        ;;                     (.toArray (WGPUBindGroupLayoutEntryByReference.) (count bindings)))

        ;;        _ (doseq [[i binding] (map-indexed vector bindings)]
        ;;            (raw/merge->WGPUBindGroupLayoutEntry
        ;;             (aget entries-arr i)
        ;;             (cond
        ;;               (:buffer binding)
        ;;               {:binding i
        ;;                :visibility raw/WGPUShaderStage_Compute
        ;;                #_(bit-or 
        ;;                   raw/WGPUShaderStage_Vertex
        ;;                   raw/WGPUShaderStage_Fragment)
                       
        ;;                :buffer (raw/map->WGPUBufferBindingLayout
        ;;                         {:type (cond
        ;;                                  (:Uniform (:usage binding))
        ;;                                  raw/WGPUBufferBindingType_Uniform

        ;;                                  (:Storage (:usage binding))
        ;;                                  raw/WGPUBufferBindingType_Storage

        ;;                                  :else (throw (ex-info "Unknown Binding type"
        ;;                                                        {:binding binding})))
        ;;                          :minBindingSize (:size binding)}
        ;;                         )}
        ;;               (:texture-view binding)
        ;;               {:binding i
        ;;                :visibility raw/WGPUShaderStage_Fragment
        ;;                :texture (raw/map->WGPUTextureBindingLayout
        ;;                          {:sampleType raw/WGPUTextureSampleType_Float
        ;;                           :viewDimension raw/WGPUTextureDimension_2D})}

        ;;               (:sampler binding)
        ;;               {:binding i
        ;;                :visibility raw/WGPUShaderStage_Fragment
        ;;                :sampler (raw/map->WGPUSamplerBindingLayout
        ;;                          {:type raw/WGPUSamplerBindingType_Filtering})}

        ;;               :else (throw (ex-info "Unknown binding type"
        ;;                                     {:binding binding})))))]
        ;;    (raw/map->WGPUPipelineLayoutDescriptor*
        ;;     {:label (->memory "pipeline layout")
        ;;      :bindGroupLayoutCount 1
        ;;      :bindGroupLayouts
        ;;      (PointerByReference.
        ;;       (raw/wgpuDeviceCreateBindGroupLayout
        ;;        device
        ;;        (raw/map->WGPUBindGroupLayoutDescriptor*
        ;;         {:label (->memory "layouts")
        ;;          :entries (aget entries-arr 0)
        ;;          :entryCount (alength entries-arr)})))})))


        render-pipeline-descriptor
        (ref!
         (raw/map->WGPURenderPipelineDescriptor*
          {:vertex (raw/map->WGPUVertexState
                    {:module shader
                     :entryPoint (->memory vertex-entry-point)})
           :primitive
           (raw/map->WGPUPrimitiveState
            {:topology raw/WGPUPrimitiveTopology_TriangleList
             :cullMode raw/WGPUCullMode_Back})
           :depthStencil
           (raw/map->WGPUDepthStencilState*
            {:stencilReadMask 0
             :stencilWriteMask 0
             :depthBias 0
             :depthBiasSlopeScale 0
             :depthBiasClamp 0
             :depthWriteEnabled 1
             :depthCompare raw/WGPUCompareFunction_Less
             :format raw/WGPUTextureFormat_Depth24Plus
             :stencilBack (raw/map->WGPUStencilFaceState
                           {:compare raw/WGPUCompareFunction_Always
                            :failOp  raw/WGPUStencilOperation_Keep
                            :depthFailOp raw/WGPUStencilOperation_Keep
                            :passOp raw/WGPUStencilOperation_Keep})
             :stencilFront (raw/map->WGPUStencilFaceState
                            {:compare raw/WGPUCompareFunction_Always
                             :failOp  raw/WGPUStencilOperation_Keep
                             :depthFailOp raw/WGPUStencilOperation_Keep
                             :passOp raw/WGPUStencilOperation_Keep})})
           :fragment
           (ref!
            (raw/map->WGPUFragmentState*
             {:module shader
              :entryPoint (->memory fragment-entry-point)
              :targetCount 1
              :targets (ref!
                        (raw/map->WGPUColorTargetState*
                         {:format texture-format
                          :blend
                          (raw/map->WGPUBlendState*
                           {:color
                            (raw/map->WGPUBlendComponent
                             {:srcFactor raw/WGPUBlendFactor_SrcAlpha
                              :dstFactor raw/WGPUBlendFactor_OneMinusSrcAlpha
                              :operation raw/WGPUBlendOperation_Add})
                            :alpha
                            (raw/map->WGPUBlendComponent
                             {:srcFactor raw/WGPUBlendFactor_Zero
                              :dstFactor raw/WGPUBlendFactor_One
                              :operation raw/WGPUBlendOperation_Add})})
                          ,
                          :writeMask raw/WGPUColorWriteMask_All})
                        ,)}))
           :multisample
           (raw/map->WGPUMultisampleState
            {:count (int 1)
             :mask (int -1)
             :alphaToCoverageEnabled (int 0)})
           ,}))
        pipeline (raw/wgpuDeviceCreateRenderPipeline device render-pipeline-descriptor)

        bind-group (when (seq bindings)
                     (let [bind-group-layout (raw/wgpuRenderPipelineGetBindGroupLayout pipeline group)

                           ^objects
                           entries-arr (ref!
                                        (.toArray (WGPUBindGroupEntryByReference.) (count bindings)))
                           _ (doseq [[i buffer] (map-indexed vector bindings)]
                               (let [^WGPUBindGroupEntryByReference
                                     entry (aget entries-arr i)]
                                 (cond

                                   (:buffer buffer)
                                   (doto entry
                                     (.writeField "binding" (int i))
                                     (.writeField "buffer" (:buffer buffer))
                                     (.writeField "offset" 0)
                                     (.writeField "size" (:size buffer)))

                                   (:texture-view buffer)
                                   (doto entry
                                     (.writeField "binding" (int i))
                                     (.writeField "textureView" (:texture-view buffer)))

                                   (:sampler buffer)
                                   (doto entry
                                     (.writeField "binding" (int i))
                                     (.writeField "sampler" (:sampler buffer)))

                                   :else (throw (ex-info "Unknown buffer type"
                                                         {:buffer buffer})))))
                           bind-group (raw/wgpuDeviceCreateBindGroup
                                       device
                                       (ref!
                                        (doto (WGPUBindGroupDescriptorByReference.)
                                          (.writeField "label" (->memory "bind_group"))
                                          (.writeField "layout" bind-group-layout)
                                          (.writeField "entryCount" (long (alength entries-arr)))
                                          (.writeField "entries" (aget entries-arr 0)))))
                           ]
                       bind-group))

        targetTextureDesc (raw/map->WGPUTextureDescriptor*
                           {:label (->memory "Render texture")
                            :dimension raw/WGPUTextureDimension_2D
                            :size (raw/map->WGPUExtent3D
                                   {:width width
                                    :height height
                                    :depthOrArrayLayers 1})
                            :format texture-format
                            :mipLevelCount 1
                            :sampleCount 1
                            :usage (bit-or raw/WGPUTextureUsage_RenderAttachment raw/WGPUTextureUsage_CopySrc)})

        targetTexture (raw/wgpuDeviceCreateTexture device targetTextureDesc )

        targetTextureViewDesc
        (raw/map->WGPUTextureViewDescriptor*
         {:label (->memory "Render texture view")
          :baseArrayLayer 0
          :arrayLayerCount 1
          :baseMipLevel 0
          :mipLevelCount 1
          :aspect raw/WGPUTextureAspect_All})

        targetTextureView (raw/wgpuTextureCreateView targetTexture targetTextureViewDesc)

        ;; depthTexture = device.createTexture({
        ;;   size: [canvasTexture.width, canvasTexture.height],
        ;;   format: 'depth24plus',
        ;;   usage: GPUTextureUsage.RENDER_ATTACHMENT,
        ;; });
        depthTexture (raw/wgpuDeviceCreateTexture
                      device
                      (raw/map->WGPUTextureDescriptor*
                       {:label (->memory "depth2")
                        :size (raw/map->WGPUExtent3D
                               {:width width
                                :height height
                                :depthOrArrayLayers 1})
                        :mipLevelCount 1
                        :sampleCount 1
                        ;; :viewFormatCount 1
                        ;; :viewFormats (IntByReference. raw/WGPUTextureFormat_Depth24Plus)
                        :dimension raw/WGPUTextureDimension_2D
                        :format raw/WGPUTextureFormat_Depth24Plus
                        :usage raw/WGPUTextureUsage_RenderAttachment}))

        depth-texture-view (raw/wgpuTextureCreateView
                            depthTexture
                            (raw/map->WGPUTextureViewDescriptor*
                             {:label (->memory "Render texture view")
                              :aspect raw/WGPUTextureAspect_DepthOnly
                              :baseArrayLayer 0
                              :arrayLayerCount 1
                              :baseMipLevel 0
                              :mipLevelCount 1
                              :format raw/WGPUTextureFormat_Depth24Plus
                              ;;:dimension raw/WGPUTextureDimension_2D
                              }) )

        command-encoder (raw/wgpuDeviceCreateCommandEncoder
                         device
                         (raw/map->WGPUCommandEncoderDescriptor*
                          {:label (->memory "command_encoder")}))

        render-pass (raw/wgpuCommandEncoderBeginRenderPass
                     command-encoder
                     (raw/map->WGPURenderPassDescriptor*
                      {:colorAttachments
                       (raw/map->WGPURenderPassColorAttachment*
                        {:view targetTextureView
                         :resolveTarget nil
                         :loadOp raw/WGPULoadOp_Clear
                         :storeOp raw/WGPUStoreOp_Store
                         :clearValue (raw/map->WGPUColor
                                      {:r 0.9
                                       :g 0.1
                                       :b 0.2
                                       :a 1.0})})
                       :colorAttachmentCount 1
                       :depthStencilAttachment
                       (raw/map->WGPURenderPassDepthStencilAttachment*
                        {:depthClearValue 1.0
                         :depthLoadOp raw/WGPULoadOp_Clear
                         :depthStoreOp raw/WGPUStoreOp_Store
                         :view depth-texture-view})}))
        _ (raw/wgpuRenderPassEncoderSetPipeline render-pass pipeline)
        _ (when bind-group
            (raw/wgpuRenderPassEncoderSetBindGroup render-pass group bind-group 0 nil))
        _ (raw/wgpuRenderPassEncoderDraw render-pass vertex-count instance-count 0 0)
        _ (raw/wgpuRenderPassEncoderEnd render-pass)
        _ (raw/wgpuRenderPassEncoderRelease render-pass)

        command-buffer (raw/wgpuCommandEncoderFinish
                        command-encoder
                        (raw/map->WGPUCommandBufferDescriptor*
                         {:label (->memory "command_buffer")}))


        _ (raw/wgpuQueueSubmit queue 1 (doto (PointerByReference.)
                                         (.setValue command-buffer)))

        _ (raw/wgpuCommandEncoderRelease command-encoder)
        _ (raw/wgpuCommandBufferRelease command-buffer)

        buf (save-texture ctx targetTexture width height)
        buf-img (->buffered-image buf width height)]

    (vreset! refs nil)

    (raw/wgpuRenderPipelineRelease pipeline)
    (raw/wgpuTextureViewRelease depth-texture-view)
    (raw/wgpuTextureDestroy depthTexture)
    (raw/wgpuTextureRelease depthTexture)
    (raw/wgpuTextureViewRelease targetTextureView)
    (raw/wgpuTextureDestroy targetTexture)
    (raw/wgpuTextureRelease targetTexture)

    buf-img))

(defn copy-from-buffer [ctx buffer]
  (raw/wgpuBufferMapAsync (:buffer buffer) raw/WGPUMapMode_Read 0 (:size buffer)
                          (fn [status user-data]
                            nil)
                          nil)
  (raw/wgpuDevicePoll (:device ctx) 1 nil)
  (let [^Pointer ret (raw/wgpuBufferGetMappedRange (:buffer buffer) 0 (:size buffer))]
    (case (:type buffer)
      :i32 (.getIntArray ret 0 (:length buffer))
      :u32 (.getIntArray ret 0 (:length buffer))
      :f32 (.getFloatArray ret 0 (:length buffer)))))

(defn copy [ctx from to]
  (let [refs (volatile! #{})
        ref! (fn [o]
               (vswap! refs conj o)
               o)
        command-encoder (raw/wgpuDeviceCreateCommandEncoder
                         (:device ctx)
                         (ref!
                          (doto (WGPUCommandEncoderDescriptorByReference.)
                            (.writeField "label" (->memory "command_encoder")))))
        compute-pass-encoder (raw/wgpuCommandEncoderBeginComputePass
                              command-encoder
                              (ref!
                               (doto (WGPUComputePassDescriptorByReference.)
                                 (.writeField "label" (->memory "compute_pass")))))
        _ (raw/wgpuCommandEncoderCopyBufferToBuffer command-encoder
                                                    (:buffer from)
                                                    0
                                                    (:buffer to)
                                                    0
                                                    (min (:size to)
                                                         (:size from)))
        command-buffer (raw/wgpuCommandEncoderFinish
                        command-encoder
                        (doto (WGPUCommandBufferDescriptorByReference.)
                          (.writeField "label" (->memory "command_buffer"))))
        _ (raw/wgpuQueueSubmit (:queue ctx) 1 (doto (PointerByReference.)
                                                (.setValue command-buffer)))]
    (vreset! refs nil)
    nil))

(defn -main [& args]
  (prn (create-buffer (create-context)
                      {:size 10
                       :usage #{:MapRead :CopyDst}})))


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
         [[:label ->memory]
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
                 (raw/map->WGPUTextureDescriptor* texture-descriptor))]
    (clean-ptr texture raw/wgpuTextureRelease)
    texture))

(defn create-texture-view [texture texture-view-descriptor]
  (let [texture-view-descriptor
        (convert-kws
         texture-view-descriptor
         [[:label ->memory]
          [:aspect kw->texture-aspect]
          [:format kw->texture-format]
          [:dimension kw->texture-dimension]])

        texture-view (raw/wgpuTextureCreateView
                      texture
                      (raw/map->WGPUTextureViewDescriptor*
                       texture-view-descriptor))
        ]
    (clean-ptr texture-view raw/wgpuTextureViewRelease [texture])
    texture-view))

(defn poll [device wait?]
  (raw/wgpuDevicePoll device (if wait? 1 0)  nil))


(defn create-bind-group [device bind-group-layout bindings]
  (let [entries (raw/struct-array
                 :clong/WGPUBindGroupEntry
                 (into []
                       (map-indexed
                        (fn [i binding]
                          (cond
                            (:buffer binding)
                            {:binding i
                             :buffer (:buffer binding)
                             :offset 0
                             :size (:size binding)}

                            (:texture-view binding)
                            {:binding i
                             :textureView (:texture-view binding)}

                            (:sampler binding)
                            {:binding i
                             :sampler (:sampler binding)}

                            :else (throw (ex-info "Unknown buffer type"
                                                  {:binding binding})))))
                       bindings))

        bind-group (raw/wgpuDeviceCreateBindGroup
                    device
                    (raw/map->WGPUBindGroupDescriptor*
                     {:layout bind-group-layout
                      :entryCount (count bindings)
                      :entries entries}
                     ))]
    (clean-ptr bind-group raw/wgpuBindGroupRelease)
    bind-group))

(defn encode-compute-pass [ctx command-encoder pass])
(defn encode-render-pass [ctx command-encoder pass]
  (let [draws (:draws pass)
        pass (:render-pass pass)

        render-pass (raw/wgpuCommandEncoderBeginRenderPass
                     command-encoder
                     (raw/map->WGPURenderPassDescriptor*
                      (merge
                       pass
                       {:colorAttachmentCount (count (:colorAttachments pass))
                        :colorAttachments
                        (raw/struct-array
                         :clong/WGPURenderPassColorAttachment
                         (into []
                               (map (fn [color-attachment]
                                      (convert-kws
                                       color-attachment
                                       [[:loadOp kw->load-op]
                                        [:storeOp kw->store-op]
                                        [:clearValue raw/map->WGPUColor]])))
                               (:colorAttachments pass)))
                        :depthStencilAttachment
                        (raw/map->WGPURenderPassDepthStencilAttachment*
                         (convert-kws
                          (:depthStencilAttachment pass)
                          [[:depthLoadOp kw->load-op]
                           [:depthStoreOp kw->store-op]]))})))]
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
    nil))

(defn submit [ctx passes]

  (let [group 0
        device (:device ctx)
        queue (:queue ctx)

        command-encoder (raw/wgpuDeviceCreateCommandEncoder
                         device
                         (raw/map->WGPUCommandEncoderDescriptor*
                          {}))

        _ (doseq [pass passes]
            (cond
              (:render-pass pass)
              (encode-render-pass ctx command-encoder pass)

              (:compute-pass pass)
              (encode-compute-pass ctx command-encoder pass)

              :else (throw (ex-info "Unrecognized pass"
                                    {:pass pass}))))

        command-buffer (raw/wgpuCommandEncoderFinish
                        command-encoder
                        (raw/map->WGPUCommandBufferDescriptor*
                         {}))



        _ (raw/wgpuQueueSubmit queue 1 (doto (PointerByReference.)
                                         (.setValue command-buffer)))

        _ (raw/wgpuCommandEncoderRelease command-encoder)
        _ (raw/wgpuCommandBufferRelease command-buffer)]
    nil))







(defn create-pipeline [ctx pipeline-descriptor]
  (let [
        ;; convert strings to Memory for some key paths
        pipeline-descriptor
        (convert-kws
         pipeline-descriptor
         [[[:vertex :entryPoint] ->memory]
          [[:fragment :entryPoint] ->memory]])

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
          (raw/map->WGPUBlendState*
           (convert-kws m
                        [[:color ->blend-component]
                         [:alpha ->blend-component]])))

        pipeline-descriptor
        (update pipeline-descriptor
                :fragment
                (fn [fragment]
                  (merge
                   fragment
                   {:targets (raw/struct-array
                              :clong/WGPUColorTargetState
                              (into []
                                    (map (fn [target]
                                           (convert-kws target
                                                        [[:format kw->texture-format]
                                                         [:blend ->blend-state]
                                                         [:writeMask kw->color-write-mask]])))
                                    (:targets fragment)))
                    :targetCount (count (:targets fragment))})))

        descriptor*
        (raw/map->WGPURenderPipelineDescriptor*
         {:vertex (raw/map->WGPUVertexState
                   (:vertex pipeline-descriptor))
          :primitive
          (raw/map->WGPUPrimitiveState
           (convert-kws
            (:primitive pipeline-descriptor)
            [[:topology kw->primitive-topology]
             [:cullMode kw->cull-mode]]))
          :depthStencil
          (raw/map->WGPUDepthStencilState*
             (convert-kws
              (:depthStencil pipeline-descriptor)
              [[:depthCompare kw->compare-function]
               [:format kw->texture-format]
               [:stencilBack ->stencil]
               [:stencilFront ->stencil]]))
          :fragment
          (raw/map->WGPUFragmentState*
           (:fragment pipeline-descriptor))
          :multisample
          (raw/map->WGPUMultisampleState
           (:multisample pipeline-descriptor))
          ,})
        pipeline (raw/wgpuDeviceCreateRenderPipeline (:device ctx) descriptor*)

        ;; hang onto shader modules
        refs (volatile! (set [(get-in pipeline-descriptor [:vertex :module])
                              (get-in pipeline-descriptor [:fragment :module])]))]
    (assert pipeline)
    (clean-ptr pipeline
               (fn [p]
                 (raw/wgpuRenderPipelineRelease p)
                 (vreset! refs nil)))
    pipeline))



