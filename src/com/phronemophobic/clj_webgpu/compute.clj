(ns com.phronemophobic.clj-webgpu.compute
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


(defn ^:private write-field [^Structure o field val]
  (.writeField o field val)
  o)


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

(defonce ^:private refs (atom #{}))
(defn ^:private ref! [o]
  (swap! refs conj o)
  o)

(defn ^:private  ->memory [o]
  (ref!
   (case (.getCanonicalName ^Class (type o))
     "java.lang.String" (let [^String o o
                              buf (.getBytes o "utf-8")
                              len (alength buf)]
                          (doto (Memory. (inc len))
                            (.write 0 buf 0 len)
                            (.setByte len 0)))

     "byte[]" (let [^bytes o o]
                (doto (Memory. (alength o))
                  (.write 0 o 0 (alength o)))))))

(defn create-context []
  (let [instance (raw/wgpuCreateInstance nil)
        adapter* (promise)
        _ (raw/wgpuInstanceRequestAdapter instance nil
                                          (fn [status adapter message user-date]
                                            (deliver adapter* adapter))
                                          nil)
        device* (promise)
        _ (raw/wgpuAdapterRequestDevice @adapter* nil
                                        (fn [status device message user-data]
                                          (deliver device* device))
                                        nil)]
    {:instance instance
     :adapter @adapter*
     :device @device*
     :queue (raw/wgpuDeviceGetQueue @device*)})
  )

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
                     label (write-field "label" (->memory label)))]
    (merge
     m
     {:buffer (raw/wgpuDeviceCreateBuffer (:device ctx) descriptor)
      :size size})))

(defn create-shader [ctx {:keys [src label]}]
  (let [chain (doto (WGPUChainedStruct.)
                (.writeField "sType" raw/WGPUSType_ShaderModuleWGSLDescriptor))

        ^WGPUShaderModuleWGSLDescriptorByReference
        next-in-chain (ref!
                       (doto (WGPUShaderModuleWGSLDescriptorByReference.)
                         (.writeField "chain" chain)
                         (.writeField "code" (->memory src))))

        descriptor
        (doto (WGPUShaderModuleDescriptorByReference.)
          (.writeField "nextInChain" (.getPointer next-in-chain)))

        descriptor (cond-> descriptor
                     label (write-field "label" (->memory label)))

        shader (raw/wgpuDeviceCreateShaderModule (:device ctx)
                                                 descriptor)]
    ;; I'm not sure this is required, but the purpose
    ;; is to prevent the unlikely gc of these before the shader is created.
    (identity [chain descriptor])
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

(defn dispatch [ctx {:keys [entry-point
                            shader
                            group
                            bindings
                            workgroups
                            ]
                     }]
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
                       (.writeField "entryCount" (long (alength entries-arr)
                                                  ))
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
    
    (vswap! refs identity)
    nil))

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
    (vswap! refs identity)
    nil))

(defn -main [& args]
  (prn (create-buffer (create-context)
                      {:size 10
                       :usage #{:MapRead :CopyDst}})))
