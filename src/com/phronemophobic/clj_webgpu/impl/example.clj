(ns com.phronemophobic.clj-webgpu.impl.example
  (:require 
   [com.phronemophobic.clj-webgpu.impl.raw :as raw])
  (:import
   java.io.PushbackReader
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.LongByReference
   com.sun.jna.ptr.ByteByReference
   com.sun.jna.Structure


   (com.phronemophobic.clj_webgpu.impl.raw.structs
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

;; ideas for reducing boilerplate
;; https://www.answer.ai/posts/2024-07-11--gpu-cpp.html

;; spir-v to wgsl?
;; https://github.com/beehive-lab/beehive-spirv-toolkit
;; https://www.youtube.com/watch?v=pd_Px8LaHvw&t=28578s

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
                (.write 0 o 0 (alength o))))))

;; static void handle_request_adapter(WGPURequestAdapterStatus status,
;;                                    WGPUAdapter adapter, char const *message,
;;                                    void *userdata) {
;;   UNUSED(status)
;;   UNUSED(message)
;;   *(WGPUAdapter *)userdata = adapter;
;; }
;; static void handle_request_device(WGPURequestDeviceStatus status,
;;                                   WGPUDevice device, char const *message,
;;                                   void *userdata) {
;;   UNUSED(status)
;;   UNUSED(message)
;;   *(WGPUDevice *)userdata = device;
;; }
;; static void handle_buffer_map(WGPUBufferMapAsyncStatus status, void *userdata) {
;;   UNUSED(userdata)
;;   printf(LOG_PREFIX " buffer_map status=%#.8x\n", status);
;; }

;; int main(int argc, char *argv[]) {
;;   UNUSED(argc)
;;   UNUSED(argv)
;;   frmwrk_setup_logging(WGPULogLevel_Warn);

;;   uint32_t numbers[] = {1, 2, 3, 4};
;;   uint32_t numbers_size = sizeof(numbers);
;;   uint32_t numbers_length = numbers_size / sizeof(uint32_t);

(defn int-memory [nums]
  (let [nums (int-array nums)
        mem (Memory. (* (alength nums)
                        4))]
    (.write mem 0 nums 0 (alength nums) )
    mem))

(def nums [1 2 3 4])
(def numbers (int-memory nums))
(def numbers-size (.size numbers))
(def numbers-length (count nums))

;;   WGPUInstance instance = wgpuCreateInstance(NULL);
;;   assert(instance);
(def instance (raw/wgpuCreateInstance nil))

;;   WGPUAdapter adapter = NULL;
;;   wgpuInstanceRequestAdapter(instance, NULL, handle_request_adapter,
;;                              (void *)&adapter);
;;   assert(adapter);

(raw/wgpuInstanceRequestAdapter instance nil
                                    (fn [status adapter* message user-date]
                                      (prn status message)
                                      (def adapter adapter*))
                                    nil)


;;   WGPUDevice device = NULL;
;;   wgpuAdapterRequestDevice(adapter, NULL, handle_request_device,
;;                            (void *)&device);
;;   assert(device);
(raw/wgpuAdapterRequestDevice adapter nil
                              (fn [status device* message user-data]
                                (prn (.getString (.getPointer message) 0 "utf-8"))
                                (def device device*))
                              nil)

;;   WGPUQueue queue = wgpuDeviceGetQueue(device);
;;   assert(queue);
(def queue (raw/wgpuDeviceGetQueue device))



;; WGPUShaderModule frmwrk_load_shader_module(WGPUDevice device,
;;                                            const char *name) {
;;   FILE *file = NULL;
;;   char *buf = NULL;
;;   WGPUShaderModule shader_module = NULL;

;;   file = fopen(name, "rb");
;;   if (!file) {
;;     perror("fopen");
;;     goto cleanup;
;;   }

;;   if (fseek(file, 0, SEEK_END) != 0) {
;;     perror("fseek");
;;     goto cleanup;
;;   }
;;   long length = ftell(file);
;;   if (length == -1) {
;;     perror("ftell");
;;     goto cleanup;
;;   }
;;   if (fseek(file, 0, SEEK_SET) != 0) {
;;     perror("fseek");
;;     goto cleanup;
;;   }

;;   buf = malloc(length + 1);
;;   assert(buf);
;;   fread(buf, 1, length, file);
;;   buf[length] = 0;

;;   shader_module = wgpuDeviceCreateShaderModule(
;;       device, &(const WGPUShaderModuleDescriptor){
;;                   .label = name,
;;                   .nextInChain =
;;                       (const WGPUChainedStruct *)&(
;;                           const WGPUShaderModuleWGSLDescriptor){
;;                           .chain =
;;                               (const WGPUChainedStruct){
;;                                   .sType = WGPUSType_ShaderModuleWGSLDescriptor,
;;                               },
;;                           .code = buf,
;;                       },
;;               });

;; cleanup:
;;   if (file)
;;     fclose(file);
;;   if (buf)
;;     free(buf);
;;   return shader_module;
;; }


(defn load-shader-module [device label code]
  (let [
        descriptor
        (doto (WGPUShaderModuleDescriptorByReference.)
          (.writeField "label" (->memory label))
          (.writeField "nextInChain"
                       (.getPointer
                        (doto (WGPUShaderModuleWGSLDescriptorByReference.)
                          (.writeField "chain"
                                       (doto (WGPUChainedStruct.)
                                         (.writeField "sType" raw/WGPUSType_ShaderModuleWGSLDescriptor)))
                          (.writeField "code" (->memory code))))))
        ]
    
    (raw/wgpuDeviceCreateShaderModule device
                                      descriptor))
  )

;;   WGPUShaderModule shader_module =
;;       frmwrk_load_shader_module(device, "shader.wgsl");
;;   assert(shader_module);
(def shader-module (load-shader-module device "myshader" (slurp "shaders/shader.wgsl")))

;;   WGPUBuffer staging_buffer = wgpuDeviceCreateBuffer(
;;       device, &(const WGPUBufferDescriptor){
;;                   .label = "staging_buffer",
;;                   .usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst,
;;                   .size = numbers_size,
;;                   .mappedAtCreation = false,
;;               });
;;   assert(staging_buffer);

(def staging-buffer (raw/wgpuDeviceCreateBuffer device
                                                (doto (WGPUBufferDescriptorByReference.)
                                                (.writeField "label" (->memory "staging_buffer"))
                                                (.writeField "usage" (int
                                                                      (bit-or raw/WGPUBufferUsage_MapRead raw/WGPUBufferUsage_CopyDst)))
                                                (.writeField "size" numbers-size)
                                                (.writeField "mappedAtCreation" (int 0)))))

;;   WGPUBuffer storage_buffer = wgpuDeviceCreateBuffer(
;;       device, &(const WGPUBufferDescriptor){
;;                   .label = "storage_buffer",
;;                   .usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst |
;;                            WGPUBufferUsage_CopySrc,
;;                   .size = numbers_size,
;;                   .mappedAtCreation = false,
;;               });
;;   assert(storage_buffer);
(def storage-buffer
  (raw/wgpuDeviceCreateBuffer device
                              (doto (WGPUBufferDescriptorByReference.)
                                (.writeField "label" (->memory "storage-buffer"))
                                (.writeField "usage" (int
                                                      (bit-or raw/WGPUBufferUsage_Storage raw/WGPUBufferUsage_CopyDst raw/WGPUBufferUsage_CopySrc)))
                                (.writeField "size" numbers-size)
                                (.writeField "mappedAtCreation" (int 0)))))

;;   WGPUComputePipeline compute_pipeline = wgpuDeviceCreateComputePipeline(
;;       device, &(const WGPUComputePipelineDescriptor){
;;                   .label = "compute_pipeline",
;;                   .compute =
;;                       (const WGPUProgrammableStageDescriptor){
;;                           .module = shader_module,
;;                           .entryPoint = "main",
;;                       },
;;               });
;;   assert(compute_pipeline);
(def compute-pipeline
  (raw/wgpuDeviceCreateComputePipeline
   device
   (doto (WGPUComputePipelineDescriptorByReference.)
     (.writeField "label" (->memory "compute_pipeline"))
     (.writeField "compute"
                  (doto (WGPUProgrammableStageDescriptor.)
                    (.writeField "module" shader-module)
                    (.writeField "entryPoint" (->memory "main")))))
                                                           
                                       ))

;;   WGPUBindGroupLayout bind_group_layout =
;;       wgpuComputePipelineGetBindGroupLayout(compute_pipeline, 0);
;;   assert(bind_group_layout);
(def bind-group-layout
  (raw/wgpuComputePipelineGetBindGroupLayout compute-pipeline 0))

;;   WGPUBindGroup bind_group = wgpuDeviceCreateBindGroup(
;;       device, &(const WGPUBindGroupDescriptor){
;;                   .label = "bind_group",
;;                   .layout = bind_group_layout,
;;                   .entryCount = 1,
;;                   .entries =
;;                       (const WGPUBindGroupEntry[]){
;;                           (const WGPUBindGroupEntry){
;;                               .binding = 0,
;;                               .buffer = storage_buffer,
;;                               .offset = 0,
;;                               .size = numbers_size,
;;                           },
;;                       },
;;               });



(def bind-group (raw/wgpuDeviceCreateBindGroup
                 device
                 (doto (WGPUBindGroupDescriptorByReference.)
                   (.writeField "label" (->memory "bind_group"))
                   (.writeField "layout" bind-group-layout)
                   (.writeField "entryCount" 1)
                   (.writeField "entries"
                                (doto (WGPUBindGroupEntryByReference.)
                                  (.writeField "binding" (int 0))
                                  (.writeField "buffer" storage-buffer)
                                  (.writeField "offset" 0)
                                  (.writeField "size" numbers-size))))))
;;   assert(bind_group);

;;   WGPUCommandEncoder command_encoder = wgpuDeviceCreateCommandEncoder(
;;       device, &(const WGPUCommandEncoderDescriptor){
;;                   .label = "command_encoder",
;;               });
;;   assert(command_encoder);
(def command-encoder (raw/wgpuDeviceCreateCommandEncoder
                     device
                     (doto (WGPUCommandEncoderDescriptorByReference.)
                       (.writeField "label" (->memory "command_encoder")))
                     ))

;;   WGPUComputePassEncoder compute_pass_encoder =
;;       wgpuCommandEncoderBeginComputePass(command_encoder,
;;                                          &(const WGPUComputePassDescriptor){
;;                                              .label = "compute_pass",
;;                                          });
;;   assert(compute_pass_encoder);
(def compute-pass-encoder (raw/wgpuCommandEncoderBeginComputePass
                           command-encoder
                           (doto (WGPUComputePassDescriptorByReference.)
                             (.writeField "label" (->memory "compute_pass")))))

;;   wgpuComputePassEncoderSetPipeline(compute_pass_encoder, compute_pipeline);
(raw/wgpuComputePassEncoderSetPipeline compute-pass-encoder compute-pipeline)

;;   wgpuComputePassEncoderSetBindGroup(compute_pass_encoder, 0, bind_group, 0,
;;                                      NULL);
(raw/wgpuComputePassEncoderSetBindGroup compute-pass-encoder 0 bind-group 0 nil)
;;   wgpuComputePassEncoderDispatchWorkgroups(compute_pass_encoder, numbers_length,
;;                                            1, 1);
(raw/wgpuComputePassEncoderDispatchWorkgroups compute-pass-encoder numbers-length 1 1)
;;   wgpuComputePassEncoderEnd(compute_pass_encoder);
(raw/wgpuComputePassEncoderEnd compute-pass-encoder)

;;   wgpuCommandEncoderCopyBufferToBuffer(command_encoder, storage_buffer, 0,
;;                                        staging_buffer, 0, numbers_size);
(raw/wgpuCommandEncoderCopyBufferToBuffer command-encoder
                                          storage-buffer
                                          0
                                          staging-buffer
                                          0
                                          numbers-size)

;;   WGPUCommandBuffer command_buffer = wgpuCommandEncoderFinish(
;;       command_encoder, &(const WGPUCommandBufferDescriptor){
;;                            .label = "command_buffer",
;;                        });
;;   assert(command_buffer);
(def command-buffer (raw/wgpuCommandEncoderFinish
                     command-encoder
                     (doto (WGPUCommandBufferDescriptorByReference.)
                       (.writeField "label" (->memory "command_buffer")))
                     ))

;;   wgpuQueueWriteBuffer(queue, storage_buffer, 0, &numbers, numbers_size);
(raw/wgpuQueueWriteBuffer queue storage-buffer 0 numbers numbers-size)
;;   wgpuQueueSubmit(queue, 1, &command_buffer);
(raw/wgpuQueueSubmit queue 1 (doto (PointerByReference.)
                               (.setValue command-buffer)))

;;   wgpuBufferMapAsync(staging_buffer, WGPUMapMode_Read, 0, numbers_size,
;;                      handle_buffer_map, NULL);
(raw/wgpuBufferMapAsync staging-buffer raw/WGPUMapMode_Read 0 numbers-size
                        (fn [status user-data]
                          (prn "buffer status" status))
                        nil)
;;   wgpuDevicePoll(device, true, NULL);
(raw/wgpuDevicePoll device 1  nil)


;;   uint32_t *buf =
;;       (uint32_t *)wgpuBufferGetMappedRange(staging_buffer, 0, numbers_size);
;;   assert(buf);
(def ret (raw/wgpuBufferGetMappedRange staging-buffer 0 numbers-size))

(prn (seq (.getIntArray ret 0 numbers-length)))
;;   printf("times: [%d, %d, %d, %d]\n", buf[0], buf[1], buf[2], buf[3]);

;;   wgpuBufferUnmap(staging_buffer);
;;   wgpuCommandBufferRelease(command_buffer);
;;   wgpuComputePassEncoderRelease(compute_pass_encoder);
;;   wgpuCommandEncoderRelease(command_encoder);
;;   wgpuBindGroupRelease(bind_group);
;;   wgpuBindGroupLayoutRelease(bind_group_layout);
;;   wgpuComputePipelineRelease(compute_pipeline);
;;   wgpuBufferRelease(storage_buffer);
;;   wgpuBufferRelease(staging_buffer);
;;   wgpuShaderModuleRelease(shader_module);
;;   wgpuQueueRelease(queue);
;;   wgpuDeviceRelease(device);
;;   wgpuAdapterRelease(adapter);
;;   wgpuInstanceRelease(instance);
;;   return EXIT_SUCCESS;
;; }


(defn -main [& args])
