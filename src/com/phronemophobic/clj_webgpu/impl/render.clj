(ns com.phronemophobic.clj-webgpu.impl.render
  (:require 
   [com.phronemophobic.clj-webgpu.impl.raw :as raw]
   [clojure.java.io :as io])
  (:import
   java.io.PushbackReader
   java.awt.image.BufferedImage
   javax.imageio.ImageIO
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.LongByReference
   com.sun.jna.ptr.IntByReference
   com.sun.jna.ptr.ByteByReference
   com.sun.jna.Structure)
  (:gen-class)
  )


(defonce refs (atom #{}))
(defn ref! [o]
  (swap! refs conj o)
  o)

(defn ->memory [o]
  (ref!
   (case (.getCanonicalName (type o))
     "java.lang.String" (let [buf (.getBytes o "utf-8")
                              len (alength buf)]
                          (doto (Memory. (inc len))
                            (.write 0 buf 0 len)
                            (.setByte len 0)))

     "byte[]" (doto (Memory. (alength o))
                (.write 0 o 0 (alength o)))

     "int[]" (doto (IntByReference.)
               (.setPointer
                (ref!
                 (doto (Memory. (* 4 (alength o)))
                   (.write 0 o 0 (alength o)))))))))

;; https://eliemichel.github.io/LearnWebGPU/advanced-techniques/headless.html
;; https://github.com/eliemichel/LearnWebGPU-Code/tree/step030-headless

;; 	Instance instance = createInstance(InstanceDescriptor{});
;; 	if (!instance) {
;; 		std::cerr << "Could not initialize WebGPU!" << std::endl;
;; 		return 1;
;; 	}
(def instance (raw/wgpuCreateInstance nil))

;; 	std::cout << "Requesting adapter..." << std::endl;
;; 	RequestAdapterOptions adapterOpts;
;; 	adapterOpts.compatibleSurface = nullptr;
;; 	//                              ^^^^^^^ This was 'surface'
;; 	Adapter adapter = instance.requestAdapter(adapterOpts);
;; 	std::cout << "Got adapter: " << adapter << std::endl;
(raw/wgpuInstanceRequestAdapter instance nil
                                (fn [status adapter* message user-date]
                                  (prn status message)
                                  (def adapter adapter*))
                                nil)

;; 	std::cout << "Requesting device..." << std::endl;
;; 	DeviceDescriptor deviceDesc;
;; 	deviceDesc.label = "My Device";
;; 	deviceDesc.requiredFeaturesCount = 0;
;; 	deviceDesc.requiredLimits = nullptr;
;; 	deviceDesc.defaultQueue.label = "The default queue";
;; 	Device device = adapter.requestDevice(deviceDesc);
;; 	std::cout << "Got device: " << device << std::endl;

(raw/wgpuAdapterRequestDevice adapter nil
                              (fn [status device* message user-data]
                                (prn (.getString (.getPointer message) 0 "utf-8"))
                                (def device device*))
                              nil)

(prn device)

;; 	// Add an error callback for more debug info
;; 	auto h = device.setUncapturedErrorCallback([](ErrorType type, char const* message) {
;; 		std::cout << "Device error: type " << type;
;; 		if (message) std::cout << " (message: " << message << ")";
;; 		std::cout << std::endl;
;; 	});


;; 	Queue queue = device.getQueue();
(def queue (raw/wgpuDeviceGetQueue device))

(defn error-handler [type message user]
  (prn (.getString (.getPointer message) 0 "utf-8")))

(raw/wgpuDeviceSetUncapturedErrorCallback device
                                          error-handler
                                          nil)


;; 	// No more swap chain, but still a target format and a target texture
;; 	TextureFormat swapChainFormat = TextureFormat::RGBA8UnormSrgb;

(def swapChainFormat raw/WGPUTextureFormat_RGBA8UnormSrgb)

(def targetTextureDesc
  (raw/map->WGPUTextureDescriptorByReference
   {:label (->memory "Render texture")
    :dimension raw/WGPUTextureDimension_2D
    :size (raw/map->WGPUExtent3D
           {:width 640
            :height 480
            :depthOrArrayLayers 1})
    :format swapChainFormat
    :mipLevelCount 1
    :sampleCount 1
    :usage (bit-or raw/WGPUTextureUsage_RenderAttachment raw/WGPUTextureUsage_CopySrc)}))

(def targetTexture (raw/wgpuDeviceCreateTexture device targetTextureDesc ))


;; 	TextureDescriptor targetTextureDesc;
;; 	targetTextureDesc.label = "Render texture";
;; 	targetTextureDesc.dimension = TextureDimension::_2D;
;; 	// Any size works here, this is the equivalent of the window size
;; 	targetTextureDesc.size = { 640, 480, 1 };
;; 	// Use the same format here and in the render pipeline's color target
;; 	targetTextureDesc.format = swapChainFormat;
;; 	// No need for MIP maps
;; 	targetTextureDesc.mipLevelCount = 1;
;; 	// You may set up supersampling here
;; 	targetTextureDesc.sampleCount = 1;
;; 	// At least RenderAttachment usage is needed. Also add CopySrc to be able
;; 	// to retrieve the texture afterwards.
;; 	targetTextureDesc.usage = TextureUsage::RenderAttachment | TextureUsage::CopySrc;
;; 	targetTextureDesc.viewFormats = nullptr;
;; 	targetTextureDesc.viewFormatCount = 0;
;; 	Texture targetTexture = device.createTexture(targetTextureDesc);

(def targetTextureViewDesc
  (raw/map->WGPUTextureViewDescriptorByReference
   {:label (->memory "Render texture view")
    :baseArrayLayer 0
    :arrayLayerCount 1
    :baseMipLevel 0
    :mipLevelCount 1
    :aspect raw/WGPUTextureAspect_All})
  
  ,)


(def targetTextureView (raw/wgpuTextureCreateView targetTexture targetTextureViewDesc))
;; 	TextureViewDescriptor targetTextureViewDesc;
;; 	targetTextureViewDesc.label = "Render texture view";
;; 	// Render to a single layer
;; 	targetTextureViewDesc.baseArrayLayer = 0;
;; 	targetTextureViewDesc.arrayLayerCount = 1;
;; 	// Render to a single mip level
;; 	targetTextureViewDesc.baseMipLevel = 0;
;; 	targetTextureViewDesc.mipLevelCount = 1;
;; 	// Render to all channels
;; 	targetTextureViewDesc.aspect = TextureAspect::All;
;; 	TextureView targetTextureView = targetTexture.createView(targetTextureViewDesc);

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
          ;; @fragment fn fs() -> @location(0) vec4f {


;; 	std::cout << "Creating shader module..." << std::endl;
;; 	const char* shaderSource = R"(
;; @vertex
;; fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> @builtin(position) vec4<f32> {
;; 	var p = vec2f(0.0, 0.0);
;; 	if (in_vertex_index == 0u) {
;; 		p = vec2f(-0.5, -0.5);
;; 	} else if (in_vertex_index == 1u) {
;; 		p = vec2f(0.5, -0.5);
;; 	} else {
;; 		p = vec2f(0.0, 0.5);
;; 	}
;; 	return vec4f(p, 0.0, 1.0);
;; }

;; @fragment
;; fn fs_main() -> @location(0) vec4f {
;;     return vec4f(0.0, 0.4, 1.0, 1.0);
;; }
;; )";

(defn load-shader-module [device label code]
  (let [descriptor
        (raw/map->WGPUShaderModuleDescriptorByReference
         {:label (->memory label)
          :nextInChain
          (raw/map->WGPUShaderModuleWGSLDescriptorByReference
           {:chain (raw/map->WGPUChainedStruct
                    {:sType raw/WGPUSType_ShaderModuleWGSLDescriptor})
            :code (->memory code)})})]
    
    (raw/wgpuDeviceCreateShaderModule device
                                      descriptor))
  )

;; 	ShaderModuleDescriptor shaderDesc;
;; #ifdef WEBGPU_BACKEND_WGPU
;; 	shaderDesc.hintCount = 0;
;; 	shaderDesc.hints = nullptr;
;; #endif

;; 	// Use the extension mechanism to load a WGSL shader source code
;; 	ShaderModuleWGSLDescriptor shaderCodeDesc;
;; 	// Set the chained struct's header
;; 	shaderCodeDesc.chain.next = nullptr;
;; 	shaderCodeDesc.chain.sType = SType::ShaderModuleWGSLDescriptor;
;; 	// Connect the chain
;; 	shaderDesc.nextInChain = &shaderCodeDesc.chain;

;; 	// Setup the actual payload of the shader code descriptor
;; 	shaderCodeDesc.code = shaderSource;

;; 	ShaderModule shaderModule = device.createShaderModule(shaderDesc);
;; 	std::cout << "Shader module: " << shaderModule << std::endl;

(def shaderModule (load-shader-module device  "shader thing" shaderSource))

;; 	std::cout << "Creating render pipeline..." << std::endl;
;; 	RenderPipelineDescriptor pipelineDesc;

(def pipelineDesc
  (raw/map->WGPURenderPipelineDescriptorByReference
   {:vertex (raw/map->WGPUVertexState
             {:module shaderModule
              :entryPoint (->memory "vs_main")})
    :primitive
    (raw/map->WGPUPrimitiveState
     {:topology raw/WGPUPrimitiveTopology_TriangleList})
    ;; (.writeField "stripIndexFormat" raw/WGPUIndexFormat_Undefined)
    ;; (.writeField "frontFace" raw/WGPUFrontFace_CCW)
    ;; (.writeField "cullMode" raw/WGPUCullMode_None)

    
    ,
    :fragment
    (ref!
     (raw/map->WGPUFragmentStateByReference
      {:module shaderModule
       :entryPoint (->memory "fs_main")
       :targetCount 1
       :targets (ref!
                 (raw/map->WGPUColorTargetStateByReference
                  {:format swapChainFormat
                   :blend
                   (raw/map->WGPUBlendStateByReference
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
    
    ,})



  ,)

;; 	// Vertex fetch
;; 	// (We don't use any input buffer so far)
;; 	pipelineDesc.vertex.bufferCount = 0;
;; 	pipelineDesc.vertex.buffers = nullptr;

;; 	// Vertex shader
;; 	pipelineDesc.vertex.module = shaderModule;
;; 	pipelineDesc.vertex.entryPoint = "vs_main";
;; 	pipelineDesc.vertex.constantCount = 0;
;; 	pipelineDesc.vertex.constants = nullptr;




;; 	// Primitive assembly and rasterization
;; 	// Each sequence of 3 vertices is considered as a triangle
;; 	pipelineDesc.primitive.topology = PrimitiveTopology::TriangleList;
;; 	// We'll see later how to specify the order in which vertices should be
;; 	// connected. When not specified, vertices are considered sequentially.
;; 	pipelineDesc.primitive.stripIndexFormat = IndexFormat::Undefined;
;; 	// The face orientation is defined by assuming that when looking
;; 	// from the front of the face, its corner vertices are enumerated
;; 	// in the counter-clockwise (CCW) order.
;; 	pipelineDesc.primitive.frontFace = FrontFace::CCW;
;; 	// But the face orientation does not matter much because we do not
;; 	// cull (i.e. "hide") the faces pointing away from us (which is often
;; 	// used for optimization).
;; 	pipelineDesc.primitive.cullMode = CullMode::None;




;; 	// Fragment shader
;; 	FragmentState fragmentState;
;; 	pipelineDesc.fragment = &fragmentState;
;; 	fragmentState.module = shaderModule;
;; 	fragmentState.entryPoint = "fs_main";
;; 	fragmentState.constantCount = 0;
;; 	fragmentState.constants = nullptr;


;; 	// Configure blend state
;; 	BlendState blendState;
;; 	// Usual alpha blending for the color:
;; 	blendState.color.srcFactor = BlendFactor::SrcAlpha;
;; 	blendState.color.dstFactor = BlendFactor::OneMinusSrcAlpha;
;; 	blendState.color.operation = BlendOperation::Add;
;; 	// We leave the target alpha untouched:
;; 	blendState.alpha.srcFactor = BlendFactor::Zero;
;; 	blendState.alpha.dstFactor = BlendFactor::One;
;; 	blendState.alpha.operation = BlendOperation::Add;


;; 	ColorTargetState colorTarget;
;; 	colorTarget.format = swapChainFormat;
;; 	colorTarget.blend = &blendState;
;; 	colorTarget.writeMask = ColorWriteMask::All; // We could write to only some of the color channels.




;; 	// We have only one target because our render pass has only one output color
;; 	// attachment.
;; 	fragmentState.targetCount = 1;
;; 	fragmentState.targets = &colorTarget;


;; 	// Depth and stencil tests are not used here
;; 	pipelineDesc.depthStencil = nullptr;


;; 	// Multi-sampling
;; 	// Samples per pixel
;; 	pipelineDesc.multisample.count = 1;
;; 	// Default value for the mask, meaning "all bits on"
;; 	pipelineDesc.multisample.mask = ~0u;
;; 	// Default value as well (irrelevant for count = 1 anyways)
;; 	pipelineDesc.multisample.alphaToCoverageEnabled = false;


;; 	// Pipeline layout
;; 	pipelineDesc.layout = nullptr;

;; 	RenderPipeline pipeline = device.createRenderPipeline(pipelineDesc);
;; 	std::cout << "Render pipeline: " << pipeline << std::endl;

(def pipeline (raw/wgpuDeviceCreateRenderPipeline device pipelineDesc))


;; 	{ // Mock main "loop"

;; 		// Instead of swapChain.getCurrentTextureView()
;; 		TextureView nextTexture = targetTextureView;
;; 		if (!nextTexture) {
;; 			std::cerr << "Cannot acquire next swap chain texture" << std::endl;
;; 			return 1;
;; 		}
(def nextTexture targetTextureView)

;; 		CommandEncoderDescriptor commandEncoderDesc;
;; 		commandEncoderDesc.label = "Command Encoder";
;; 		CommandEncoder encoder = device.createCommandEncoder(commandEncoderDesc);



(def command-encoder (raw/wgpuDeviceCreateCommandEncoder
                      device
                      (raw/map->WGPUCommandEncoderDescriptorByReference
                       {:label (->memory "command_encoder")})
                      ))


(def renderPass
  (raw/wgpuCommandEncoderBeginRenderPass
   command-encoder
   (raw/map->WGPURenderPassDescriptorByReference
    {:colorAttachments
     (raw/map->WGPURenderPassColorAttachmentByReference
      {:view nextTexture
       :resolveTarget nil
       :loadOp raw/WGPULoadOp_Clear
       :storeOp raw/WGPUStoreOp_Store
       :clearValue (raw/map->WGPUColor
                    {:r 0.9
                     :g 0.1
                     :b 0.2
                     :a 1.0})})
     :colorAttachmentCount 1
     :depthStencilAttachment nil})
   ;; (.writeField "timestampWriteCount" 0)
   ;; (.writeField "timestampWrites" nil)
   
   ,))

(raw/wgpuRenderPassEncoderSetPipeline
 renderPass
 pipeline)

(raw/wgpuRenderPassEncoderDraw renderPass 3 1 0 0)

(raw/wgpuRenderPassEncoderEnd renderPass)

(raw/wgpuRenderPassEncoderRelease renderPass)


;; (raw/wgpuTextureRelease nextTexture)

(def command-buffer
  (raw/wgpuCommandEncoderFinish
   command-encoder
   (raw/map->WGPUCommandBufferDescriptorByReference
    {:label (->memory "command_buffer")})))

(raw/wgpuCommandEncoderRelease command-encoder)
(raw/wgpuQueueSubmit queue 1 (doto (PointerByReference.)
                               (.setValue command-buffer)))

(raw/wgpuCommandBufferRelease command-buffer)



;; (raw/wgpuRenderPassEncoderDraw)
;; 		RenderPassDescriptor renderPassDesc;

;; 		RenderPassColorAttachment renderPassColorAttachment;
;; 		renderPassColorAttachment.view = nextTexture;
;; 		renderPassColorAttachment.resolveTarget = nullptr;
;; 		renderPassColorAttachment.loadOp = LoadOp::Clear;
;; 		renderPassColorAttachment.storeOp = StoreOp::Store;
;; 		renderPassColorAttachment.clearValue = Color{ 0.9, 0.1, 0.2, 1.0 };
;; 		renderPassDesc.colorAttachmentCount = 1;
;; 		renderPassDesc.colorAttachments = &renderPassColorAttachment;

;; 		renderPassDesc.depthStencilAttachment = nullptr;
;; 		renderPassDesc.timestampWriteCount = 0;
;; 		renderPassDesc.timestampWrites = nullptr;
;; 		RenderPassEncoder renderPass = encoder.beginRenderPass(renderPassDesc);

;; 		// In its overall outline, drawing a triangle is as simple as this:
;; 		// Select which render pipeline to use
;; 		renderPass.setPipeline(pipeline);
;; 		// Draw 1 instance of a 3-vertices shape
;; 		renderPass.draw(3, 1, 0, 0);

;; 		renderPass.end();
;; 		renderPass.release();
		
;; 		nextTexture.release();

;; 		CommandBufferDescriptor cmdBufferDescriptor;
;; 		cmdBufferDescriptor.label = "Command buffer";
;; 		CommandBuffer command = encoder.finish(cmdBufferDescriptor);
;; 		encoder.release();
;; 		queue.submit(command);
;; 		command.release();

(defmacro with-tile [[tile-bind img] & body]
  `(let [img# ~img
         ~tile-bind (.getWritableTile img# 0 0)]
     (try
       ~@body
       (finally
         (.releaseWritableTile img# 0 0)))))

(defn ->buffered-image
  [^Memory buf width height]
  (let [

        img (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
        ;; 4 bytes per pixel
        linesize (* 4 width)

        get-buf (fn [y] (.getByteArray buf (* linesize y) linesize))]

    (with-tile [wraster img]
      (doseq [y (range height)]

        (.setDataElements wraster 0 y width 1
                          (get-buf y))))
    img))

(defn save-png [bufimg f]
  (with-open [os (clojure.java.io/output-stream f)]
    (ImageIO/write ^BufferedImage bufimg "png" os)))

;; https://github.com/eliemichel/LearnWebGPU-Code/blob/6f5e4cf1297b4f06a1e3659b0ecc1ab581709757/save_image.h#L240
(defn save-texture [device texture]
  (let [
        width (-> targetTextureDesc :size :width)
        height (-> targetTextureDesc :size :height)
        
        ;; 	auto device = m_device;
        ;; 	auto width = m_width;
        ;; 	auto height = m_height;
        ;; 	auto pixelBuffer = m_pixelBuffer;
        ;; 	auto pixelBufferDesc = m_pixelBufferDesc;

        encoder (raw/wgpuDeviceCreateCommandEncoder
                 device
                 (raw/map->WGPUCommandEncoderDescriptorByReference
                  {:label (->memory "command_encoder")})
                 )
        ;; 	// Start encoding the commands
        ;; 	CommandEncoder encoder = device.createCommandEncoder(Default);

        source (raw/map->WGPUImageCopyTextureByReference
                {:texture texture})
        
        pixelBuffer-size (* 4 width height)
        pixelBuffer (raw/wgpuDeviceCreateBuffer
                     device
                     (raw/map->WGPUBufferDescriptorByReference
                      {:usage (int
                               (bit-or raw/WGPUBufferUsage_MapRead raw/WGPUBufferUsage_CopyDst))
                       :size pixelBuffer-size
                       :mappedAtCreation (int 0)}))
        
	;; BufferDescriptor pixelBufferDesc = Default;
	;; pixelBufferDesc.mappedAtCreation = false;
	;; pixelBufferDesc.usage = BufferUsage::MapRead | BufferUsage::CopyDst;
	;; pixelBufferDesc.size = 4 * width * height;
	;; Buffer pixelBuffer = device.createBuffer(pixelBufferDesc);
        

        destination (raw/map->WGPUImageCopyBufferByReference
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
           (raw/map->WGPUExtent3DByReference
            {:width (int width)
             :height (int height)
             :depthOrArrayLayers (int 1)})
           )
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
                        (raw/map->WGPUCommandBufferDescriptorByReference
                         {:label (->memory "command_buffer")}))
        
        _ (raw/wgpuQueueSubmit queue 1 (doto (PointerByReference.)
                                         (.setValue command-buffer)))

        _ (raw/wgpuBufferMapAsync pixelBuffer raw/WGPUMapMode_Read 0 pixelBuffer-size
                                  (fn [status user-data]
                                    (prn "buffer status" status))
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
    bs
    )

  
  )

;; 		// Instead of swapChain.present()
;; 		saveTexture("output.png", device, targetTexture);
;; 		//saveTextureView("output.png", device, nextTexture, targetTexture.getWidth(), targetTexture.getHeight());
;; 	}


(def mem
 (save-texture device targetTexture))

(-> mem
    (->buffered-image (-> targetTextureDesc :size :width)
                      (-> targetTextureDesc :size :height))
    (save-png (io/file "output.png")))

;; 	pipeline.release();
;; 	shaderModule.release();
;; 	swapChain.release();
;; 	device.release();
;; 	adapter.release();
;; 	instance.release();
;; 	surface.release();
;; 	glfwDestroyWindow(window);
;; 	glfwTerminate();
(defn -main [& args])

(comment

  ((requiring-resolve 'com.phronemophobic.easel/run))
  ,)
