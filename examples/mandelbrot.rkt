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

(require vulkan/unsafe
         ffi/unsafe)

(define (two-step-alloc _t f [who 'two-step-alloc])
  (define pnum (malloc _uint32_t 'atomic))
  (f pnum #f)
  (define num (ptr-ref pnum _uint32_t))
  (when (= num 0)
    (error who "Expected more than zero elements from enumeration."))
  (define buf (malloc (* num (ctype-sizeof _t)) 'atomic))
  (f pnum buf)
  (values num buf))

(define (create _t f)
  (define p (malloc _t 'atomic))
  (f p)
  (ptr-ref p _t))

(module+ main
  (define instance (createInstance))
  (define physical-device (findPhysicalDevice instance))

  (define p (malloc _VkPhysicalDeviceProperties 'atomic))
  (memset p 0 (ctype-sizeof _VkPhysicalDeviceProperties))
  (vkGetPhysicalDeviceProperties physical-device p)

  (define props (ptr-ref p _VkPhysicalDeviceProperties))
  (writeln (VkPhysicalDeviceProperties-apiVersion props))
  (writeln (VkPhysicalDeviceProperties-driverVersion props))
  (writeln (VkPhysicalDeviceProperties-vendorID props))
  (writeln (VkPhysicalDeviceProperties-deviceID props))
  (writeln (VkPhysicalDeviceProperties-deviceType props))
  (writeln (VkPhysicalDeviceProperties-deviceName props))
  (writeln (VkPhysicalDeviceProperties-pipelineCacheUUID props))
  (writeln (VkPhysicalDeviceProperties-limits props))
  (writeln (VkPhysicalDeviceProperties-sparseProperties props))

  (vkDestroyInstance instance #f))

(define (createInstance)
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
                    0
                    #f
                    0
                    #f))

  (create _VkInstance
          (λ (p) (vkCreateInstance instinfo
                                   #f
                                   p))))

(define (findPhysicalDevice instance)
  ; List all physical devices on the system.
  (define-values (deviceCount devices)
    (two-step-alloc
     _VkPhysicalDevice
     (λ (n b) (vkEnumeratePhysicalDevices instance n b))))

  ; Optimisically assert that the first device will handle our needs.
  ; Not a good idea in a real-world scenario.
  (array-ref (ptr-ref devices
                      (_array _VkPhysicalDevice
                              deviceCount)
                      0)
             0))
