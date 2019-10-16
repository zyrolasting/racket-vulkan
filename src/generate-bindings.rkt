#lang racket/base

;---------------------------------------------------------------------------------------------------
; This module generates FFI bindings for Vulkan

(require racket/contract)
(provide
  (contract-out
    ; Return a list of datums that can be written as a Racket module.
    [generate-vulkan-bindings (-> vulkan-spec? list?)]))


(module+ main
  (require racket/list)
  (for ([datum (generate-vulkan-bindings (get-vulkan-spec 'local))])
    (writeln datum)))

;---------------------------------------------------------------------------------------------------
; Implementation
; Registry guide: https://www.khronos.org/registry/vulkan/specs/1.1/registry.html

(require racket/list
         racket/string
         "./vulkan-spec.rkt"                ; For sourcing VulkanAPI spec
         "./txexpr.rkt"                     ; For element analyis
         graph                              ; For dependency modeling
         (rename-in ffi/unsafe [-> ffi->])) ; For Racket<>C type declarations

(module+ test
  (require rackunit
           racket/list))

(define (generate-vulkan-bindings registry)
  (generate-signatures registry (curate-types registry)))

; Return types sorted such that any dependency appears
; before its dependent(s), and they appear in the groups
; ordered by the list in section 10.2.3.
(define (curate-types registry)
  ; Right now I am trusting that dependencies come before
  ; dependents in the registry for brevity. The guide
  ; does not guarentee this will happen, however.
  (define types (find-all-by-tag 'type registry))

  (define (category cat)
    (filter (λ (x) (equal? (attr-ref x 'category "")
                           cat))
            types))

  (apply append (map category '("basetype"
                                "handle"
                                "enum"
                                ; "group" Uncomment when the registry starts to use this.
                                "bitmask"
                                "funcpointer"
                                "struct"
                                "union"))))


(define (cname str)
  (string->symbol (string-append "_" str)))
(define (cnamef fmt-string . args)
  (cname (apply format fmt-string args)))

(define (generate-basetype-signature type-xexpr registry)
  (define registry-type-name (get-type-name type-xexpr))
  (define racket-id (cname registry-type-name))
  ; Note: This will produce redundant declarations like
  ; (define _void _void). Hopefully benign.
  ;
  ; The _t replacement occurs because Racket C numeric types exclude them.
  `(define ,racket-id ,(cname (string-replace registry-type-name
                                              "_t"
                                              ""))))

(module+ test
  (test-equal? "Generate basetype without _t"
               (generate-basetype-signature '(type ((category "basetype") (name "void"))))
               '(define _void _void))
  (test-equal? "Generate basetype signature with _t"
               (generate-basetype-signature '(type ((category "basetype") (name "uint32_t"))))
               '(define _uint32_t _uint32)))
 
;; ------------------------------------------------
;; C structs correspond to <type category="struct">

(define (generate-member-signature member-xexpr)
  (define children (get-elements member-xexpr))
  (define name (snatch-cdata 'name member-xexpr))
  (define undecorated-type (snatch-cdata 'type member-xexpr))
  (define characters (string->list (shrink-wrap-cdata member-xexpr)))

  (define pointer-depth
    (count (λ (ch) (or (char=? #\* ch)
                       (char=? #\[ ch))) ; TODO: Should this be wrapped as an array type?
           characters))

  ; Wrap pointer declarations equal to the number of '*'s
  (define type
    (for/fold ([sig (cname undecorated-type)])
              ([i (in-range pointer-depth)])
      `(_cpointer ,sig)))

  `(,(string->symbol name) ,type))

(define (generate-struct-signature struct-xexpr [registry #f])
  `(define-cstruct ,(cname (get-type-name struct-xexpr))
     . ,(map generate-member-signature
            (get-elements-of-tag 'member
                                 struct-xexpr))))

(define (generate-union-signature union-xexpr [registry #f])
  `(define ,(cname (get-type-name union-xexpr))
     (make-union-type
      . ,(map generate-member-signature
              (get-elements-of-tag 'member
                                   union-xexpr)))))

(module+ test
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
    (test-equal? "(generate-union-signature)"
                 (generate-union-signature example-union-xexpr)
                 '(define _VkClearColorValue
                    (make-union-type (float32 (_cpointer _float))
                                     (int32 (_cpointer _int32_t))
                                     (uint32 (_cpointer _uint32_t))))))
  

(module+ test
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

    (test-equal? "(generate-struct-signature)"
                 (generate-struct-signature example-struct-xexpr)
                 `(define-cstruct _VkDeviceCreateInfo
                    (sType _VkStructureType)
                    (pNext (_cpointer _void))
                    (flags _VkDeviceCreateFlags)
                    (queueCreateInfoCount _uint32_t)
                    (pQueueCreateInfos (_cpointer _VkDeviceQueueCreateInfo))
                    (enabledLayerCount _uint32_t)
                    (ppEnabledLayerNames (_cpointer (_cpointer _char)))
                    (enabledExtensionCount _uint32_t)
                    (ppEnabledExtensionNames (_cpointer (_cpointer _char)))
                    (pEnabledFeatures (_cpointer _VkPhysicalDeviceFeatures)))))

(define (generate-handle-signature handle-xexpr [registry #f])
  (define name (get-type-name handle-xexpr))
  `(define ,(cname name) (_cpointer ',(cnamef "~a_T" name))))


;; ------------------------------------------------------------------
;; Enumerations are related by <type> and <enums> elements. Some
;; <enums> elements are a list of #defines. Others are actual C enums.
;; This is the case that generates actual C enums.
(define (generate-enum-signature enum-xexpr registry)
  (define name (get-type-name enum-xexpr))
  (define enum-decl (hash-ref (collect-named-enums registry) name))

  (define pairs (map (λ (x) (cons (attr-ref x 'name)
                                  (attr-ref x 'value)))
                     (filter (λ (x) (tag=? 'enum x))
                             (get-elements enum-decl))))

  `(define ,(cname name)
     (_enum
      ',(for/fold ([decls '()])
                  ([enumerant (in-list (reverse pairs))])
          (define w/value (cons (string->number (cdr enumerant)) decls))
          (define w/= (cons '= w/value))
          (define w/all (cons (string->symbol (car enumerant)) w/=))
          w/all))))

(module+ test
  (test-case "(generate-enum-signature)"
    (define enum-registry '(registry
                            (enums ((name "VkBlendOp") (type "enum"))
                                   (enum ((value "0") (name "VK_BLEND_OP_ADD")))
                                   (enum ((value "1") (name "VK_BLEND_OP_SUBTRACT")))
                                   (enum ((value "2") (name "VK_BLEND_OP_REVERSE_SUBTRACT")))
                                   (enum ((value "3") (name "VK_BLEND_OP_MIN")))
                                   (enum ((value "4") (name "VK_BLEND_OP_MAX"))))))
    (check-equal?
     (generate-enum-signature '(type ((category "enum") (name "VkBlendOp")))
                              enum-registry)
     '(define _VkBlendOp
        (_enum '(VK_BLEND_OP_ADD = 0
                 VK_BLEND_OP_SUBTRACT = 1
                 VK_BLEND_OP_REVERSE_SUBTRACT = 2
                 VK_BLEND_OP_MIN = 3
                 VK_BLEND_OP_MAX = 4))))))

; The bitmask type names are just aliases. For now, at least.
(define (generate-bitmask-signature bitmask-xexpr [registry #f])
  (define alias (attr-ref bitmask-xexpr 'alias #f))
  `(define ,(cname (get-type-name bitmask-xexpr))
     ,(cname (or alias
                 (snatch-cdata 'type
                           bitmask-xexpr
                           #:children-only? #t)))))

(module+ test
  (test-equal? "Generate bitmask signature"
               (generate-bitmask-signature '(type ((category "bitmask"))
                                                  "typedef "
                                                  (type "VkFlags")
                                                  " "
                                                  (name "VkFramebufferCreateFlags")))
               '(define _VkFramebufferCreateFlags _VkFlags)))

(define (generate-funcpointer-signature funcpointer-xexpr [registry #f])
  '(define funcpointer ...))

(define (generate-signatures registry ordered)
  (define category=>proc
    `#hash(("basetype"    . ,generate-basetype-signature)
           ("handle"      . ,generate-handle-signature)
           ("enum"        . ,generate-enum-signature)
           ("bitmask"     . ,generate-bitmask-signature)
           ("funcpointer" . ,generate-funcpointer-signature)
           ("struct"      . ,generate-struct-signature)
           ("union"       . ,generate-union-signature)))

  (for/list ([type (in-list ordered)])
    (define category (attr-ref type 'category ""))
    ((hash-ref category=>proc category)
     type registry)))
