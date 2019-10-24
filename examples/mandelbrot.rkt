#lang racket

; Credit @Erkaman for original under MIT License
; https://github.com/Erkaman/vulkan_minimal_compute
;
; Adapted to Racket to test Vulkan FFI bindings and demonstrate that
; hand-porting C++ to Racket is a feasible means to that end.
;
; "The application launches a compute shader that renders the mandelbrot set,
; by rendering it into a storage buffer. The storage buffer is then read from
; the GPU, and saved as .png." -- @Erkaman
;
; I am changing the comments and program structure as it comes to this module.

(require racket/runtime-path
         vulkan/unsafe
         ffi/unsafe
         ffi/cvector)

(define (enable-validation-layer?)
  (equal? #"true"
          (environment-variables-ref
           (current-environment-variables)
           #"RACKET_VULKAN_ENABLE_VALIDATION_LAYER")))

(define (two-step-alloc _t f [who 'two-step-alloc])
  (define pnum (malloc _uint32_t 'atomic))
  (f pnum #f)
  (define num (ptr-ref pnum _uint32_t))
  (when (= num 0)
    (error who "Expected more than zero elements from enumeration."))
  (define buf (malloc (* num (ctype-sizeof _t)) 'atomic))
  (f pnum buf)
  (values num buf))

(define (make-zero _t [_pointer-type #f])
  (define size (ctype-sizeof _t))
  (define p (malloc 'atomic size))
  (memset p 0 size)
  (cpointer-push-tag! p _t)
  (if _pointer-type
      (cast p
            _pointer
            _pointer-type)
      p))

(define (create _t f)
  (define p (malloc 'atomic _t))
  (f p)
  (ptr-ref p _t))

(define (_char_array->bytes array)
  (apply bytes (filter (λ (b) (> b 0))
                       (sequence->list (in-array array)))))

(define-cstruct _pixel
  ([r _float]
   [g _float]
   [b _float]
   [a _float]))

(module+ main
  (define with-validation (enable-validation-layer?))
  (define layers (get-layers with-validation))
  (define extensions (get-extensions with-validation))
  (define instance (create-instance layers extensions with-validation))

  (when with-validation
    (register-debug-callback instance))

  (define physical-device (find-physical-device instance))
  (define queue-family-index (get-compute-queue-family-index physical-device))
  (define logical-device (create-logical-device instance layers physical-device queue-family-index))
  (define queue (get-queue logical-device queue-family-index))

  (define width 3200)
  (define height 2400)
  (define buffer-size (* (ctype-sizeof _pixel) width height))
  (define buffer (create-buffer logical-device buffer-size))

  (define buffer-memory (allocate-for-buffer physical-device
                                             logical-device
                                             buffer))

  (define-values (descriptor-set-layout/p descriptor-set-layout)
    (create-descriptor-set-layout logical-device))

  (define-values (descriptor-pool/p descriptor-pool)
    (create-descriptor-pool logical-device))

  (define-values (descriptor-set/p descriptor-set)
    (create-descriptor-set logical-device descriptor-set-layout/p descriptor-pool))

  (define shader-module (create-shader-module logical-device))

  (define pipeline-layout (create-pipeline-layout logical-device descriptor-set-layout/p))

  (define pipeline (create-compute-pipeline logical-device shader-module pipeline-layout))

  (vkFreeMemory logical-device buffer-memory #f)
  (vkDestroyBuffer logical-device buffer #f)
  (vkDestroyShaderModule logical-device shader-module #f)
  (vkDestroyDescriptorPool logical-device descriptor-pool #f)
  (vkDestroyDescriptorSetLayout logical-device descriptor-set-layout #f)
  (vkDestroyPipelineLayout logical-device pipeline-layout #f)
  (vkDestroyPipeline logical-device pipeline #f)
  (vkDestroyDevice logical-device #f)
  (vkDestroyInstance instance #f))

(define (get-layers with-validation)
  (define layer-count/p (malloc _uint32_t 'atomic))
  (vkEnumerateInstanceLayerProperties layer-count/p #f)
  (define layer-count (ptr-ref layer-count/p _uint32_t))
  (define layer-properties/p (malloc (* layer-count (ctype-sizeof _VkLayerProperties)) 'atomic))
  (vkEnumerateInstanceLayerProperties layer-count/p layer-properties/p)

  (define validation-layer-name #"VK_LAYER_LUNARG_standard_validation")
  (define (validation-layer-supported?)
    (for/fold ([support #f])
              ([offset (in-range layer-count)])
      #:break support
      (or support
          (let ([props (ptr-ref layer-properties/p _VkLayerProperties offset)])
            (bytes=? validation-layer-name
                     (_char_array->bytes (VkLayerProperties-layerName props)))))))

  (when (and with-validation
             (not (validation-layer-supported?)))
    (error "The validation layer is not available on this system."))

  (if with-validation
      (cvector _bytes/nul-terminated validation-layer-name)
      (cvector _bytes/nul-terminated)))


(define (get-extensions with-validation)
  (define extension-count/p (malloc _uint32_t 'atomic))
  (vkEnumerateInstanceExtensionProperties #f extension-count/p #f)
  (define extension-count (ptr-ref extension-count/p _uint32_t))
  (define extension-properties/p (malloc (* extension-count (ctype-sizeof _VkExtensionProperties)) 'atomic))
  (vkEnumerateInstanceExtensionProperties #f extension-count/p extension-properties/p)

  (define debug-extension-name #"VK_EXT_debug_report")
  (define (debug-extension-supported?)
    (for/fold ([support #f])
              ([offset (in-range extension-count)])
      #:break support
      (or support
          (let ([props (ptr-ref extension-properties/p _VkExtensionProperties offset)])
            (bytes=? debug-extension-name
                     (_char_array->bytes (VkExtensionProperties-extensionName props)))))))

  (when (and with-validation
             (not (debug-extension-supported?)))
    (error "The validation layer is not available on this system."))

  (if with-validation
      (cvector _bytes/nul-terminated debug-extension-name)
      (cvector _bytes/nul-terminated)))


(define (get-queue logical-device queue-family-index)
  (define queue/p (malloc _VkQueue 'atomic))
  (vkGetDeviceQueue logical-device queue-family-index 0 queue/p)
  (ptr-ref queue/p _VkQueue))


;; Creates the means to interact with a physical device
(define (create-logical-device instance
                               layers
                               physical-device
                               queue-family-index)
  (define queue-priorities/p (malloc _float))
  (ptr-set! queue-priorities/p _float 1.0)
  (cpointer-push-tag! queue-priorities/p _float)

  (define queue-create-info/p (make-zero _VkDeviceQueueCreateInfo _VkDeviceQueueCreateInfo-pointer))
  (define queue-create-info (ptr-ref queue-create-info/p _VkDeviceQueueCreateInfo))
  (set-VkDeviceQueueCreateInfo-sType! queue-create-info 'VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
  (set-VkDeviceQueueCreateInfo-queueFamilyIndex! queue-create-info queue-family-index)
  (set-VkDeviceQueueCreateInfo-queueCount! queue-create-info 1)
  (set-VkDeviceQueueCreateInfo-pQueuePriorities! queue-create-info queue-priorities/p)

  (define device-create-info/p (make-zero _VkDeviceCreateInfo))
  (define device-create-info (ptr-ref device-create-info/p _VkDeviceCreateInfo))
  (define device-features/p (make-zero _VkPhysicalDeviceFeatures _VkPhysicalDeviceFeatures-pointer))
  (set-VkDeviceCreateInfo-sType! device-create-info 'VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO)
  (set-VkDeviceCreateInfo-enabledLayerCount! device-create-info (cvector-length layers))
  (set-VkDeviceCreateInfo-ppEnabledLayerNames! device-create-info (cvector-ptr layers))
  (set-VkDeviceCreateInfo-pQueueCreateInfos! device-create-info queue-create-info/p)
  (set-VkDeviceCreateInfo-queueCreateInfoCount! device-create-info 1)
  (set-VkDeviceCreateInfo-pEnabledFeatures! device-create-info device-features/p)

  (define logical-device/p (malloc _VkDevice 'atomic))
  (vkCreateDevice physical-device device-create-info/p #f logical-device/p)

  (ptr-ref logical-device/p _VkDevice))


(define (get-compute-queue-family-index physical-device)
  (define queue-family-count/p (malloc _uint32_t 'atomic))
  (vkGetPhysicalDeviceQueueFamilyProperties physical-device queue-family-count/p #f)
  (define queue-family-count (ptr-ref queue-family-count/p _uint32_t))

  (define queue-family-properties/p (malloc (* queue-family-count
                                               (ctype-sizeof _VkQueueFamilyProperties))
                                            'atomic))

  (vkGetPhysicalDeviceQueueFamilyProperties physical-device
                                            queue-family-count/p
                                            queue-family-properties/p)

  ; Now find a family that supports compute.
  (define index
    (for/fold ([matching-family-index #f])
              ([i (in-range queue-family-count)])
      (or matching-family-index
          (let ([props (ptr-ref queue-family-properties/p
                                _VkQueueFamilyProperties
                                i)])
            (if (and (> (VkQueueFamilyProperties-queueCount props) 0)
                     (> (bitwise-and (VkQueueFamilyProperties-queueFlags props)
                                     VK_QUEUE_COMPUTE_BIT)
                        0))
                i
                #f)))))


  (when (= index queue-family-count)
    (error "No queue family supports compute operations."))

  (printf "Queue family index ~a supports compute operations.~n" index)

  index)

(define (debug-report-callback flags objectType object location messageCode pLayerPrefix pMessage pUserData)
  (printf "Debug Report: ~a: ~a~n" pLayerPrefix pMessage)
  VK_FALSE)

(define (register-debug-callback instance)
  (define drcci/p (make-zero _VkDebugReportCallbackCreateInfoEXT
                             _VkDebugReportCallbackCreateInfoEXT-pointer))
  (define drcci (ptr-ref drcci/p _VkDebugReportCallbackCreateInfoEXT))
  (set-VkDebugReportCallbackCreateInfoEXT-sType! drcci 'VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT)
  (set-VkDebugReportCallbackCreateInfoEXT-flags! drcci (bitwise-ior
                                                        VK_DEBUG_REPORT_ERROR_BIT_EXT
                                                        VK_DEBUG_REPORT_WARNING_BIT_EXT
                                                        VK_DEBUG_REPORT_INFORMATION_BIT_EXT
                                                        VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT))
  (set-VkDebugReportCallbackCreateInfoEXT-pfnCallback! drcci (function-ptr
                                                              debug-report-callback
                                                              _PFN_vkDebugReportCallbackEXT))
  (define create-debug-report-callback
    (function-ptr (vkGetInstanceProcAddr instance #"vkCreateDebugReportCallbackEXT")
                  (_cprocedure (list _VkInstance _pointer _pointer _pointer)
                               (list _VkResult))))

  (define callback/p (malloc _VkDebugReportCallbackEXT 'atomic))
  (check-vkResult (create-debug-report-callback instance
                                                drcci/p
                                                #f
                                                callback/p))
  (ptr-ref callback/p _pointer _VkDebugReportCallbackEXT))

(define (create-buffer logical-device buffer-size)
  (define buffer-create-info/p (make-zero _VkBufferCreateInfo _VkBufferCreateInfo-pointer))
  (define buffer-create-info (ptr-ref buffer-create-info/p _VkBufferCreateInfo))
  (set-VkBufferCreateInfo-sType! buffer-create-info 'VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO)
  (set-VkBufferCreateInfo-size! buffer-create-info buffer-size)
  (set-VkBufferCreateInfo-usage! buffer-create-info VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
  (set-VkBufferCreateInfo-sharingMode! buffer-create-info 'VK_SHARING_MODE_EXCLUSIVE)

  (define buffer/p (malloc _VkBuffer 'atomic))
  (vkCreateBuffer logical-device buffer-create-info/p #f buffer/p)
  (ptr-ref buffer/p _VkBuffer))

(define (allocate-for-buffer physical-device logical-device buffer)
  (define (find-memory-type type-bits properties)
    (define pdmp/p (make-zero _VkPhysicalDeviceMemoryProperties
                              _VkPhysicalDeviceMemoryProperties-pointer))
    (vkGetPhysicalDeviceMemoryProperties physical-device pdmp/p)
    (define pdmp (ptr-ref pdmp/p _VkPhysicalDeviceMemoryProperties))

    (define ordinals (range (VkPhysicalDeviceMemoryProperties-memoryTypeCount pdmp)))
    (or (index-where ordinals
                     (λ (i)
                       (and (> (bitwise-and type-bits
                                            (arithmetic-shift i 1)))
                            (= (bitwise-and
                                (VkMemoryType-propertyFlags
                                 (array-ref
                                  (VkPhysicalDeviceMemoryProperties-memoryTypes pdmp)
                                  i))
                                properties)))))
        -1))

  (define memory-requirements/p (make-zero _VkMemoryRequirements _VkMemoryRequirements-pointer))
  (vkGetBufferMemoryRequirements logical-device buffer memory-requirements/p)
  (define memory-requirements (ptr-ref memory-requirements/p _VkMemoryRequirements))

  (define memory-alloc-info/p (make-zero _VkMemoryAllocateInfo _VkMemoryAllocateInfo-pointer))
  (define memory-alloc-info (ptr-ref memory-alloc-info/p _VkMemoryAllocateInfo))
  (set-VkMemoryAllocateInfo-sType! memory-alloc-info 'VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
  (set-VkMemoryAllocateInfo-allocationSize! memory-alloc-info (VkMemoryRequirements-size memory-requirements))
  (set-VkMemoryAllocateInfo-memoryTypeIndex! memory-alloc-info
                                             (find-memory-type
                                              (VkMemoryRequirements-memoryTypeBits memory-requirements)
                                              (bitwise-ior VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
                                                           VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)))

  (define buffer-memory/p (malloc _VkDeviceMemory 'atomic))
  (vkAllocateMemory logical-device memory-alloc-info/p #f buffer-memory/p)
  (define buffer-memory (ptr-ref buffer-memory/p _VkDeviceMemory))

  (vkBindBufferMemory logical-device buffer buffer-memory 0)
  buffer-memory)


(define (create-descriptor-set-layout logical-device)
  (define dslb/p (make-zero _VkDescriptorSetLayoutBinding
                            _VkDescriptorSetLayoutBinding-pointer))
  (define dslb (ptr-ref dslb/p _VkDescriptorSetLayoutBinding))
  (set-VkDescriptorSetLayoutBinding-binding! dslb 0)
  (set-VkDescriptorSetLayoutBinding-descriptorType! dslb 'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
  (set-VkDescriptorSetLayoutBinding-descriptorCount! dslb 1)
  (set-VkDescriptorSetLayoutBinding-stageFlags! dslb VK_SHADER_STAGE_COMPUTE_BIT)

  (define dslci/p (make-zero _VkDescriptorSetLayoutCreateInfo
                             _VkDescriptorSetLayoutCreateInfo-pointer))
  (define dslci (ptr-ref dslb/p _VkDescriptorSetLayoutCreateInfo))
  (set-VkDescriptorSetLayoutCreateInfo-sType! dslci 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
  (set-VkDescriptorSetLayoutCreateInfo-bindingCount! dslci 1)
  (set-VkDescriptorSetLayoutCreateInfo-pBindings! dslci dslb/p)

  (define descriptor-set-layout/p (malloc _VkDescriptorSetLayout 'atomic))
  (vkCreateDescriptorSetLayout logical-device dslci/p #f descriptor-set-layout/p)
  (values descriptor-set-layout/p
          (ptr-ref descriptor-set-layout/p _VkDescriptorSetLayout)))


(define (create-descriptor-pool logical-device)
  (define dps/p (make-zero _VkDescriptorPoolSize _VkDescriptorPoolSize-pointer))
  (define dps (ptr-ref dps/p _VkDescriptorPoolSize))
  (set-VkDescriptorPoolSize-type! dps 'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
  (set-VkDescriptorPoolSize-descriptorCount! dps 1)

  (define dpci/p (make-zero _VkDescriptorPoolCreateInfo _VkDescriptorPoolCreateInfo-pointer))
  (define dpci (ptr-ref dpci/p _VkDescriptorPoolCreateInfo))
  (set-VkDescriptorPoolCreateInfo-sType! dpci 'VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
  (set-VkDescriptorPoolCreateInfo-maxSets! dpci 1)
  (set-VkDescriptorPoolCreateInfo-poolSizeCount! dpci 1)
  (set-VkDescriptorPoolCreateInfo-pPoolSizes! dpci dps/p)

  (define descriptor-pool/p (malloc _VkDescriptorPool 'atomic))
  (vkCreateDescriptorPool logical-device dpci/p #f descriptor-pool/p)
  (values descriptor-pool/p
          (ptr-ref descriptor-pool/p _VkDescriptorPool)))


(define (create-descriptor-set logical-device descriptor-set-layout/p descriptor-pool)
  (define dsai/p (make-zero _VkDescriptorSetAllocateInfo _VkDescriptorSetAllocateInfo-pointer))
  (define dsai (ptr-ref dsai/p _VkDescriptorSetAllocateInfo))
  (set-VkDescriptorSetAllocateInfo-sType! dsai 'VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
  (set-VkDescriptorSetAllocateInfo-descriptorPool! dsai descriptor-pool)
  (set-VkDescriptorSetAllocateInfo-descriptorSetCount! dsai 1)
  (set-VkDescriptorSetAllocateInfo-pSetLayouts! dsai descriptor-set-layout/p)

  (define descriptor-set/p (malloc _VkDescriptorSet 'atomic))
  (vkAllocateDescriptorSets logical-device dsai/p descriptor-set/p)
  (values descriptor-set/p
          (ptr-ref descriptor-set/p _VkDescriptorSet)))

(define-runtime-path here ".")
(define (create-shader-module logical-device)
  (define (read-file)
    (define byte-content
      (call-with-input-file
        (build-path here "comp.spv")
        (λ (port) (port->bytes port #:close? #f))))
    (bytes-append byte-content
                  (make-bytes
                   (modulo (bytes-length byte-content)
                           4)
                   0)))

  (define code (read-file))
  (define smci/p (make-zero _VkShaderModuleCreateInfo _VkShaderModuleCreateInfo-pointer))
  (define smci (ptr-ref smci/p _VkShaderModuleCreateInfo))
  (set-VkShaderModuleCreateInfo-sType! smci 'VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
  (set-VkShaderModuleCreateInfo-pCode! smci code)
  (set-VkShaderModuleCreateInfo-codeSize! smci (bytes-length code))

  (define compute-shader-module/p (malloc _VkShaderModule 'atomic))
  (vkCreateShaderModule logical-device smci/p #f compute-shader-module/p)
  (ptr-ref compute-shader-module/p _VkShaderModule))

(define (create-pipeline-layout logical-device descriptor-set-layout/p)
  (define plci/p (make-zero _VkPipelineLayoutCreateInfo
                            _VkPipelineLayoutCreateInfo-pointer))
  (define plci (ptr-ref plci/p _VkPipelineLayoutCreateInfo))
  (set-VkPipelineLayoutCreateInfo-sType! plci 'VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO) 
  (set-VkPipelineLayoutCreateInfo-setLayoutCount! plci 1)
  (set-VkPipelineLayoutCreateInfo-pSetLayouts! plci descriptor-set-layout/p)
  (define pipeline-layout/p (malloc _VkPipelineLayout 'atomic))
  (vkCreatePipelineLayout logical-device plci/p #f pipeline-layout/p)
  (ptr-ref pipeline-layout/p _VkPipelineLayout))

(define (create-compute-pipeline logical-device shader-module pipeline-layout)
  (define ssci/p (make-zero _VkPipelineShaderStageCreateInfo _VkPipelineShaderStageCreateInfo-pointer))
  (define ssci (ptr-ref ssci/p _VkPipelineShaderStageCreateInfo))
  (set-VkPipelineShaderStageCreateInfo-sType! ssci 'VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
  (set-VkPipelineShaderStageCreateInfo-stage! ssci 'VK_SHADER_STAGE_COMPUTE_BIT)
  (set-VkPipelineShaderStageCreateInfo-module! ssci shader-module)
  (set-VkPipelineShaderStageCreateInfo-pName! ssci #"main")

  (define pci/p (make-zero _VkComputePipelineCreateInfo _VkComputePipelineCreateInfo-pointer))
  (define pci (ptr-ref pci/p _VkComputePipelineCreateInfo))
  (set-VkComputePipelineCreateInfo-sType! pci 'VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO)
  (set-VkComputePipelineCreateInfo-stage! pci ssci)
  (set-VkComputePipelineCreateInfo-layout! pci pipeline-layout)

  (define pipeline/p (malloc _VkPipeline 'atomic))
  (vkCreateComputePipelines logical-device
                            #f
                            1
                            pci/p
                            #f
                            pipeline/p)
  (ptr-ref pipeline/p _VkPipeline))

(define (create-instance layers extensions with-validation)
  (define appInfo (make-VkApplicationInfo 'VK_STRUCTURE_TYPE_APPLICATION_INFO
                                          #f
                                          #"Mandelbrot"
                                          0
                                          #"mandelbroteng"
                                          0
                                          VK_API_VERSION_1_0))

  (define instinfo (make-VkInstanceCreateInfo
                    'VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                    #f
                    0
                    appInfo
                    (cvector-length layers)
                    (cvector-ptr layers)
                    (cvector-length extensions)
                    (cvector-ptr extensions)))

  (create _VkInstance
          (λ (p) (vkCreateInstance instinfo
                                   #f
                                   p))))



(define (find-physical-device instance)
  (define pDeviceCount (malloc _uint32_t 'atomic))
  (vkEnumeratePhysicalDevices instance pDeviceCount #f)
  (define num (ptr-ref pDeviceCount _uint32_t))
  (when (= num 0)
    (error "Expected more than zero physical devices."))
  (define size (* num (ctype-sizeof _VkPhysicalDevice)))
  (define physical-devices (malloc size 'atomic))
  (vkEnumeratePhysicalDevices instance pDeviceCount physical-devices)
  (define first-device (ptr-ref physical-devices _VkPhysicalDevice 0))
  (printf "Assuming first device is good enough.~n")
  first-device)
