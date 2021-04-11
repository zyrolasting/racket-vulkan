#lang racket/base

; Reports physical device information.
(require racket/sequence
         racket/string
         vulkan/unsafe
         ffi/unsafe)

(define (create _t f)
  (define p (malloc _t 'atomic))
  (f p)
  (ptr-ref p _t))

(define (format-uuid numbers)
  (string-upcase
   (apply format (string-replace "xxxx-xx-xx-xx-xxxxxx"
                                 "x" "~x")
          numbers)))

(define (print-physical-device-properties props)
  (define (<< name val)
    (printf "~a: ~a~n" name val))

  (<< "API Version" (let ([v (VkPhysicalDeviceProperties-apiVersion props)])
                      (format "~a.~a.~a"
                              (VK_VERSION_MAJOR v)
                              (VK_VERSION_MINOR v)
                              (VK_VERSION_PATCH v))))
  (<< "Driver Version" (VkPhysicalDeviceProperties-driverVersion props))
  (<< "Vendor ID" (VkPhysicalDeviceProperties-vendorID props))
  (<< "Device ID" (VkPhysicalDeviceProperties-deviceID props))
  (<< "Device Type" (VkPhysicalDeviceProperties-deviceType props))
  (<< "Device Name" (let ([array (VkPhysicalDeviceProperties-deviceName props)])
                      (apply string (sequence->list (sequence-map (λ (ch) (integer->char ch))
                                                                  (sequence-filter (λ (ch) (> ch 0))
                                                                                   (in-array array)))))))
  (<< "Pipeline Cache UUID"
      (let ([array (VkPhysicalDeviceProperties-pipelineCacheUUID props)])
        (format-uuid (sequence->list (in-array array))))))

(module+ main
  (define instance (createInstance))
  (define-values (numDevices pPhysicalDevices)
    (get-physical-devices instance))

  (define size (ctype-sizeof _VkPhysicalDeviceProperties))
  (define pPropsBuffer (malloc size 'atomic))
  (memset pPropsBuffer 0 size)

  (for ([i (in-range numDevices)])
    (define pPhysicalDevice (ptr-add pPhysicalDevices i _VkPhysicalDevice))
    (vkGetPhysicalDeviceProperties pPhysicalDevice pPropsBuffer)
    (define props (ptr-ref pPropsBuffer _VkPhysicalDeviceProperties))
    (print-physical-device-properties props))

  (vkDestroyInstance instance #f))

(define (createInstance)
  (define appInfo (make-VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO
                                          #f
                                          #"Physical Device Report"
                                          0
                                          #"examples"
                                          0
                                          VK_API_VERSION_1_1))

  (define instinfo (make-VkInstanceCreateInfo
                    VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
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

(define (get-physical-devices instance)
  (define pDeviceCount (malloc _uint32_t 'atomic))
  (vkEnumeratePhysicalDevices instance pDeviceCount #f)
  (define num (ptr-ref pDeviceCount _uint32_t))
  (when (= num 0)
    (error "Expected more than zero physical devices."))
  (define size (* num (ctype-sizeof _VkPhysicalDevice)))
  (define physical-devices (malloc size 'atomic))
  (vkEnumeratePhysicalDevices instance pDeviceCount physical-devices)
  (values num
          (ptr-ref physical-devices _VkPhysicalDevice 0)))
