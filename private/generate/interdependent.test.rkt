#lang racket/base

(module+ test
  (require rackunit
           racket/list
           "./shared.rkt"
           "./interdependent.rkt")

  (test-case "(generate-union-signature)"
    (define example-union-xexpr
      '(type ((category "union") (name "VkClearColorValue"))
             (member (type "float")
                     (name "float32")
                     "[4]")
             (member (type "int32_t")
                     (name "int32")
                     "[4]")
             (member (type "uint32_t")
                     (name "uint32")
                     "[4]")))
    (check-equal?
     (generate-union-signature example-union-xexpr)
     '(begin
        (define _VkClearColorValue
          (_union (_list-struct _float _float _float _float)
                  (_list-struct _int32_t _int32_t _int32_t _int32_t)
                  (_list-struct _uint32_t _uint32_t _uint32_t _uint32_t)))
        (define (VkClearColorValue-float32 u)
          (union-ref u 0))
        (define (VkClearColorValue-int32 u)
          (union-ref u 1))
        (define (VkClearColorValue-uint32 u)
          (union-ref u 2)))))


  (test-case "(generate-struct-signature)"
    ; Faithful parse of vk.xml fragment. Do not modify.
    (define example-struct-xexpr
      '(type ((category "struct")
              (name "VkDeviceCreateInfo"))
             "\n  "
             (member ((values "VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO"))
                     (type () "VkStructureType") " "
                     (name () "sType"))
             "\n  "
             (member ()
                     "const "
                     (type () "void")
                     "*     "
                     (name () "pNext"))
             "\n  "
             (member ((optional "true"))
                     (type () "VkDeviceCreateFlags")
                     "    "
                     (name () "flags"))
             "\n  "
             (member ()
                     (type () "uint32_t")
                     "        "
                     (name () "queueCreateInfoCount"))
             "\n  "
             (member ((len "queueCreateInfoCount"))
                     "const "
                     (type () "VkDeviceQueueCreateInfo")
                     "* " (name () "pQueueCreateInfos"))
             "\n  "
             (member ((optional "true"))
                     (type () "uint32_t")
                     "               "
                     (name () "enabledLayerCount"))
             "\n  "
             (member ((len "enabledLayerCount,null-terminated"))
                     "const "
                     (type () "char")
                     "* const*      "
                     (name () "ppEnabledLayerNames")
                     (comment () "Ordered list of layer names to be enabled"))
             "\n  "
             (member ((optional "true"))
                     (type () "uint32_t")
                     "               "
                     (name () "enabledExtensionCount"))
             "\n  "
             (member ((len "enabledExtensionCount,null-terminated"))
                     "const "
                     (type () "char")
                     "* const*      "
                     (name () "ppEnabledExtensionNames"))
             "\n  "
             (member ((optional "true"))
                     "const "
                     (type () "VkPhysicalDeviceFeatures")
                     "* "
                     (name () "pEnabledFeatures"))
             "\n"))

    (check-equal?
     (generate-struct-signature example-struct-xexpr)
     `(define-cstruct _VkDeviceCreateInfo
        ((sType _VkStructureType)
         (pNext _pointer)
         (flags _VkDeviceCreateFlags)
         (queueCreateInfoCount _uint32_t)
         (pQueueCreateInfos _pointer)
         (enabledLayerCount _uint32_t)
         (ppEnabledLayerNames _pointer)
         (enabledExtensionCount _uint32_t)
         (ppEnabledExtensionNames _pointer)
         (pEnabledFeatures _pointer))))


    (test-equal? "(generate-struct-signature): circular"
                 (generate-struct-signature
                  '(type ((category "struct")
                          (name "C"))
                         (member (type "C") "* " (name "pNext"))))
                 `(define-cstruct _C
                    ((pNext _pointer)))))

  (test-case "(generate-enum-signature)"
    (define enum-registry '(registry
                            (enums ((name "VkShaderStageFlagBits") (type "bitmask"))
                                   (enum ((bitpos "0") (name "VK_SHADER_STAGE_VERTEX_BIT")))
                                   (enum ((bitpos "1") (name "VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT")))
                                   (enum ((bitpos "2") (name "VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT")))
                                   (enum ((bitpos "3") (name "VK_SHADER_STAGE_GEOMETRY_BIT")))
                                   (enum ((bitpos "4") (name "VK_SHADER_STAGE_FRAGMENT_BIT")))
                                   (enum ((bitpos "5") (name "VK_SHADER_STAGE_COMPUTE_BIT")))
                                   (enum ((value "0x0000001F") (name "VK_SHADER_STAGE_ALL_GRAPHICS")))
                                   (enum ((value "0x7FFFFFFF") (name "VK_SHADER_STAGE_ALL"))))
                            (enums ((name "VkBlendOp") (type "enum"))
                                   (enum ((value "0") (name "VK_BLEND_OP_ADD")))
                                   (enum ((alias "VK_BLEND_OP_SUBTRACT") (name "VK_SHIMMED")))
                                   (enum ((value "1") (name "VK_BLEND_OP_SUBTRACT")))
                                   (enum ((value "2") (name "VK_BLEND_OP_REVERSE_SUBTRACT")))
                                   (enum ((value "3") (name "VK_BLEND_OP_MIN")))
                                   (enum ((value "4") (name "VK_BLEND_OP_MAX"))))))

    (parameterize ([enable-symbolic-enums #t])
      (check-equal?
       (generate-enum-signature '(type ((category "enum") (name "VkBlendOp")))
                                enum-registry)
       '(begin
          (define _VkBlendOp
            (_enum '(VK_BLEND_OP_ADD = 0
                                     VK_SHIMMED = 1
                                     VK_BLEND_OP_SUBTRACT = 1
                                     VK_BLEND_OP_REVERSE_SUBTRACT = 2
                                     VK_BLEND_OP_MIN = 3
                                     VK_BLEND_OP_MAX = 4)
                   _ufixint))
          (define VK_BLEND_OP_ADD 0)
          (define VK_SHIMMED 1)
          (define VK_BLEND_OP_SUBTRACT 1)
          (define VK_BLEND_OP_REVERSE_SUBTRACT 2)
          (define VK_BLEND_OP_MIN 3)
          (define VK_BLEND_OP_MAX 4)))

      (check-equal?
       (generate-enum-signature '(type ((category "enum") (name "NotPresent")))
                                enum-registry)
       '(begin (define _NotPresent (_enum '() _ufixint))))

      (check-equal?
       (generate-enum-signature '(type ((category "enum") (name "VkShaderStageFlagBits")))
                                enum-registry)
       '(begin
          (define _VkShaderStageFlagBits
            (_bitmask '(VK_SHADER_STAGE_VERTEX_BIT = 1
                                                   VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = 2
                                                   VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = 4
                                                   VK_SHADER_STAGE_GEOMETRY_BIT = 8
                                                   VK_SHADER_STAGE_FRAGMENT_BIT = 16
                                                   VK_SHADER_STAGE_COMPUTE_BIT = 32
                                                   VK_SHADER_STAGE_ALL_GRAPHICS = 31
                                                   VK_SHADER_STAGE_ALL = 2147483647)
                      _uint))
          (define VK_SHADER_STAGE_VERTEX_BIT 1)
          (define VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT 2)
          (define VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT 4)
          (define VK_SHADER_STAGE_GEOMETRY_BIT 8)
          (define VK_SHADER_STAGE_FRAGMENT_BIT 16)
          (define VK_SHADER_STAGE_COMPUTE_BIT 32)
          (define VK_SHADER_STAGE_ALL_GRAPHICS 31)
          (define VK_SHADER_STAGE_ALL 2147483647))))

    (parameterize ([enable-symbolic-enums #f])
      (check-equal?
       (generate-enum-signature '(type ((category "enum") (name "VkBlendOp")))
                                enum-registry)
       '(begin
          (define _VkBlendOp _ufixint)
          (define VK_BLEND_OP_ADD 0)
          (define VK_SHIMMED 1)
          (define VK_BLEND_OP_SUBTRACT 1)
          (define VK_BLEND_OP_REVERSE_SUBTRACT 2)
          (define VK_BLEND_OP_MIN 3)
          (define VK_BLEND_OP_MAX 4)))

      (check-equal?
       (generate-enum-signature '(type ((category "enum") (name "NotPresent")))
                                enum-registry)
       '(begin (define _NotPresent _ufixint)))

      (check-equal?
       (generate-enum-signature '(type ((category "enum") (name "VkShaderStageFlagBits")))
                                enum-registry)
       '(begin
          (define _VkShaderStageFlagBits _uint)
          (define VK_SHADER_STAGE_VERTEX_BIT 1)
          (define VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT 2)
          (define VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT 4)
          (define VK_SHADER_STAGE_GEOMETRY_BIT 8)
          (define VK_SHADER_STAGE_FRAGMENT_BIT 16)
          (define VK_SHADER_STAGE_COMPUTE_BIT 32)
          (define VK_SHADER_STAGE_ALL_GRAPHICS 31)
          (define VK_SHADER_STAGE_ALL 2147483647)))))


  (test-equal? "(generate-bitmask-signature)"
               (generate-bitmask-signature '(type ((category "bitmask"))
                                                  "typedef "
                                                  (type "VkFlags")
                                                  " "
                                                  (name "VkFramebufferCreateFlags")))
               '(define _VkFramebufferCreateFlags _VkFlags))

  (test-equal? "(generate-funcpointer-signature)"
               (generate-funcpointer-signature
                '(type ((category "funcpointer"))
                       "typedef void* (VKAPI_PTR *" (name "PFN_vkAllocationFunction") ")(\n   "
                       (type "void") "* pUserData,"
                       (type "size_t") "size,"
                       (type "size_t") "alignment,"
                       (type "VkSystemAllocationScope") "allocationScope);"))
               '(define _PFN_vkAllocationFunction (_fun _pointer
                                                        _size_t
                                                        _size_t
                                                        _VkSystemAllocationScope
                                                        ->
                                                        _pointer)))

  (test-case "(generate-maybe-wrapper)"
    (test-equal? "vkResult = #f"
                 (generate-maybe-wrapper #f 'w)
                 null)
    (test-equal? "vkResult = #t, many others"
                 (generate-maybe-wrapper #t 'w)
                 '(-> (check-vkResult r 'w))))

  (test-case "(generate-type-spec)"
    (test-equal? "Simple type"
                 (generate-type-spec
                  '(param (type "VkFlags")
                          (name "flags")))
                 '_VkFlags)
    (test-equal? "Pointer type"
                 (generate-type-spec
                  '(param "const "
                          (type "VkInstanceCreateInfo")
                          "* "
                          (name "pCreateInfo")))
                 '_pointer))

  (test-case "(generate-command-signature)"
    (define command-xexpr
      '(command
        (proto (type "VkResult") " " (name "vkCreateInstance"))
        (param "const "
               (type "VkInstanceCreateInfo")
               "* "
               (name "pCreateInfo"))
        (param ((optional "true"))
               "const "
               (type "VkAllocationCallbacks")
               "* "
               (name "pAllocator"))
        (param (type "VkInstance")
               "* "
               (name "pInstance"))))
    (parameterize ([enable-auto-check-vkresult #t])
      (test-equal? "With auto-check"
                   (generate-command-signature command-xexpr)
                   '(define-vulkan vkCreateInstance
                      (_fun _pointer
                            _pointer
                            _pointer
                            -> (r : _VkResult)
                            -> (check-vkResult r 'vkCreateInstance)))))
    (parameterize ([enable-auto-check-vkresult #f])
      (test-equal? "Without auto-check"
                   (generate-command-signature command-xexpr)
                   '(define-vulkan vkCreateInstance
                      (_fun _pointer
                            _pointer
                            _pointer
                            -> (r : _VkResult)))))))
