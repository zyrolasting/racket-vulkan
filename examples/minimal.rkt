#lang racket/base

(require vulkan/unsafe
         ffi/unsafe)

(define appinfo
  (make-VkApplicationInfo 'VK_STRUCTURE_TYPE_APPLICATION_INFO
                          #f
                          #"Minimal Vulkan"
                          0
                          #"Engine"
                          0
                          VK_API_VERSION_1_0))
(define instcreateinfo
  (make-VkInstanceCreateInfo
   'VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
   #f
   0
   appinfo
   0
   #f
   0
   #f))

(define instance-ptr (malloc _VkInstance))
(vkCreateInstance instcreateinfo #f instance-ptr)
(define instance (ptr-ref instance-ptr _VkInstance))
(vkDestroyInstance instance #f)
