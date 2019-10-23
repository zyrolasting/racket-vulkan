#lang racket/base

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

(require racket/sequence
         vulkan/unsafe
         ffi/unsafe
         ffi/cvector)

(define (enable-validation-layer?)
  (define val (environment-variables-ref
               (current-environment-variables)
               #"RACKET_VULKAN_ENABLE_VALIDATION_LAYER"))
  (and val
       (bytes=? val #"true")))

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

(module+ main
  (define with-validation (enable-validation-layer?))
  (define layers (get-layers with-validation))
  (define extensions (get-extensions with-validation))
  (define instance (create-instance layers extensions))
  (define physical-device (find-physical-device instance))
  (define queue-family-index (get-compute-queue-family-index physical-device))
  (define logical-device (create-logical-device instance layers physical-device queue-family-index))
  (define queue (get-queue logical-device queue-family-index))

  (define physical-device-props/p (make-zero _VkPhysicalDeviceProperties))
  (vkGetPhysicalDeviceProperties physical-device physical-device-props/p)
  (define props (ptr-ref physical-device-props/p _VkPhysicalDeviceProperties))

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
      (cvector _char validation-layer-name)
      (cvector _char)))

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
      (cvector _char debug-extension-name)
      (cvector _char)))


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

(define (create-instance layers extensions)

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
