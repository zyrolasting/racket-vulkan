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

  (define size (ctype-sizeof _VkPhysicalDeviceProperties))
  (displayln size)
  (define p (malloc size 'atomic))
  (memset p 0 size)

  (vkGetPhysicalDeviceProperties physical-device p)
  (define props (ptr-ref p _VkPhysicalDeviceProperties))
  (writeln (VkPhysicalDeviceProperties->list* props))

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
  (define pDeviceCount (malloc _uint32_t 'atomic))
  (vkEnumeratePhysicalDevices instance pDeviceCount #f)
  (define num (ptr-ref pDeviceCount _uint32_t))
  (when (= num 0)
    (error "Expected more than zero physical devices."))
  (define size (* num (ctype-sizeof _VkPhysicalDevice)))
  (define physical-devices (malloc size 'atomic))
  (vkEnumeratePhysicalDevices instance pDeviceCount physical-devices)
  (define first-device (ptr-ref physical-devices _VkPhysicalDevice 0))
  first-device)
