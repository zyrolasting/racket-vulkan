#lang racket/base

;---------------------------------------------------------------------------------------------------
; This module generates FFI bindings for Vulkan

(require racket/contract)
(provide
  (contract-out
    ; Return a list of datums that can be written as a Racket module.
    [generate-vulkan-bindings (-> vulkan-spec? list?)]))

(define (write-module-out signatures [out (current-output-port)])
  (parameterize ([current-output-port out])
    (displayln "#lang racket/base")
    (displayln "(provide (all-defined-out))")
    (displayln "(require ffi/unsafe ffi/unsafe/define)")
    (displayln "(define-ffi-definer define-vulkan (ffi-lib \"libvulkan\"))")
    (for ([sig signatures])
      (writeln sig))))

(module+ main
  (require racket/list)
  (write-module-out (generate-vulkan-bindings (get-vulkan-spec 'local))))


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

  ; The "define" category includes both C macros and C type declarations.
  ; Strip out the macros to disambiguate the data. Note that predicate
  ; assumes all macros start with VK_ and are all caps. Checking for
  ; '#define' in the CDATA will remove legitimate declarations.
  (define -macros (filter (λ (t)
                            (define name (get-type-name t))
                            (define category (attr-ref t 'category ""))
                            (define is-macro (and (equal? category "define")
                                                  (string-prefix? name "VK_")
                                                  (equal? (string-upcase name)
                                                          name)))
                            (not is-macro))
                          types))

  (define (already-in-racket? x)
    (member (string->symbol (get-type-name x))
            '(void float double)))

  ; Include vk_platform and top-level category-less types as basetypes.
  (define +ctypes (map (λ (x)
                         (define requires (attr-ref x 'requires ""))
                         (if (and (equal? requires "vk_platform")
                                  (not (already-in-racket? x)))
                             (attr-set x 'category "ctype")
                             x))
                       -macros))

  (define +forward-decls (map (λ (x)
                                (define requires (attr-ref x 'requires ""))
                                (define category (attr-ref x 'category ""))
                                (if (or (string-suffix? requires ".h")
                                        (member category '("struct" "union")))
                                    (attr-set x 'category "symdecl")
                                    x))
                              +ctypes))

  
  (define (category cat)
    (filter (λ (x) (equal? (attr-ref x 'category "")
                           cat))
            +forward-decls))

  (apply append (map category '("ctype"
                                "symdecl"
                                "define"
                                "basetype"
                                "handle"
                                "enum"
                                ; "group" Uncomment when the registry starts to use this.
                                "bitmask"
                                "struct"
                                "union"
                                "funcpointer"))))


(define (cname str)
  (string->symbol (string-append "_" str)))

(define (cnamef fmt-string . args)
  (cname (apply format fmt-string args)))

(define (infer-pointer-depth undecorated-type characters)
  (define pointer-depth
    (count (λ (ch) (or (char=? #\* ch)
                       (char=? #\[ ch))) ; TODO: Should this be wrapped as an array type?
           characters))

  ; Wrap pointer declarations equal to the number of '*'s
  (for/fold ([sig (cname undecorated-type)])
            ([i (in-range pointer-depth)])
    `(_cpointer ,sig)))


(define (generate-basetype-signature type-xexpr [registry #f])
  (define name (get-type-name type-xexpr))
  (define original-type (shrink-wrap-cdata
                         (findf (λ (x) (tag=? 'type x))
                                (get-elements type-xexpr))))

  `(define ,(cname name) ,(cname original-type)))

(define (generate-symdecl-signature type-xexpr [registry #f])
  (define name (get-type-name type-xexpr))
  `(define ,(cname name) ',(string->symbol name)))

; Note that this assumes C macros won't appear. If they do,
; then you'll get useless identifiers.
(define generate-define-signature
  (procedure-rename generate-symdecl-signature
                    'generate-define-signature))

(define (generate-ctype-signature type-xexpr [registry #f])
  (define registry-type-name (get-type-name type-xexpr))
  (define racket-id (cname registry-type-name))

  (define name=>existing
    #hash(("char" . "sbyte")))

  (define type-id
   (if (hash-has-key? name=>existing registry-type-name)
       (hash-ref name=>existing registry-type-name)
       ; The _t replacement occurs because Racket C numeric types exclude them,
       ; and Racket already has bindings for _size, _uint8, etc.
       (string-replace registry-type-name
                       "_t"
                       "")))

  `(define ,racket-id ,(cname type-id)))
(module+ test
  (test-equal? "Generate basetype without _t"
               (generate-ctype-signature '(type ((category "ctype") (name "void"))))
               '(define _void _void))
  (test-equal? "Generate basetype signature with _t"
               (generate-ctype-signature '(type ((category "ctype") (name "uint32_t"))))
               '(define _uint32_t _uint32)))


;; ------------------------------------------------
;; C unions correspond to <type category="union">

(define (generate-member-signature/union member-xexpr)
  (define undecorated-type (snatch-cdata 'type member-xexpr))
  (define cdata (get-all-cdata member-xexpr))
  (define characters (string->list cdata))
  (define array-size-match (regexp-match #px"\\[([^\\]]+)\\]" cdata))
  (define ctype (cname undecorated-type))
  (if array-size-match
      `(_list-struct . ,(build-list (string->number (cadr array-size-match))
                                    (λ _ ctype)))
      `(_list-struct ,ctype)))

(define (generate-union-signature union-xexpr [registry #f])
  `(define ,(cname (get-type-name union-xexpr))
     (_union
      . ,(map generate-member-signature/union
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
                    (_union (_list-struct _float _float _float _float)
                            (_list-struct _int32_t _int32_t _int32_t _int32_t)
                            (_list-struct _uint32_t _uint32_t _uint32_t _uint32_t)))))


;; ------------------------------------------------
;; C structs correspond to <type category="struct">

(define (generate-struct-signature struct-xexpr [registry #f])
  (define struct-name (get-type-name struct-xexpr))
  (define (generate-member-signature member-xexpr)
    (define name (snatch-cdata 'name member-xexpr))
    (define undecorated-type (snatch-cdata 'type member-xexpr))
    (define characters (string->list (shrink-wrap-cdata member-xexpr)))
    (define type (infer-pointer-depth (if (equal? undecorated-type struct-name)
                                          "void"
                                          undecorated-type)
                                      characters))

    `(,(string->symbol name) ,type))

  `(define-cstruct ,(cname struct-name)
     . (,(map generate-member-signature
             (get-elements-of-tag 'member
                                  struct-xexpr)))))

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
                    ((sType _VkStructureType)
                     (pNext (_cpointer _void))
                     (flags _VkDeviceCreateFlags)
                     (queueCreateInfoCount _uint32_t)
                     (pQueueCreateInfos (_cpointer _VkDeviceQueueCreateInfo))
                     (enabledLayerCount _uint32_t)
                     (ppEnabledLayerNames (_cpointer (_cpointer _char)))
                     (enabledExtensionCount _uint32_t)
                     (ppEnabledExtensionNames (_cpointer (_cpointer _char)))
                     (pEnabledFeatures (_cpointer _VkPhysicalDeviceFeatures)))))

    
    (test-equal? "(generate-struct-signature): circular"
                 (generate-struct-signature
                  '(type ((category "struct")
                          (name "C"))
                         (member (type "C") "* " (name "pNext"))))
                 `(define-cstruct _C
                    ((pNext (_cpointer _void))))))

;; ------------------------------------------------------------------
;; Handles are just pointers to forward-declared structs with private
;; definitions. We use symbols to represent them on the Racket side.

(define (generate-handle-signature handle-xexpr [registry #f])
  (define name (get-type-name handle-xexpr))
  `(define ,(cname name) (_cpointer ',(string->symbol (string-append name "_T")))))

(module+ test
  (test-equal? "(generate-handle-signature)"
               (generate-handle-signature '(type ((category "handle"))
                                                 "MAKE_HANDLE(" (name "VkDevice") ")"))
               '(define _VkDevice (_cpointer 'VkDevice_T))))


;; ------------------------------------------------------------------
;; Enumerations are related by <type> and <enums> elements. Some
;; <enums> elements are a list of #defines. Others are actual C enums.
;; This is the case that generates actual C enums.

(define (generate-enum-signature enum-xexpr registry)
  (define name (get-type-name enum-xexpr))

  ; Empty enums are possible.
  ; https://github.com/KhronosGroup/Vulkan-Docs/issues/1060
  (define enum-decl (hash-ref (collect-named-enums registry)
                              name
                              (λ _ '(enums))))

  ; Some enumerants are an alias for another enumerant.
  ; Often times the original is in the same enumeration,
  ; but that isn't explicitly guarenteed. So I search the
  ; registry for the original each time.
  (define (resolve-alias enumerant)
    (define alias (attr-ref enumerant 'alias))
    (attr-set
     (findf-txexpr registry
                  (λ (x)
                    (and (tag=? 'enum x)
                         (equal? (attr-ref x 'name "")
                                 alias))))
     'name
     (attr-ref enumerant 'name)))

  ; Pull out the intended (assumed numerical) value
  ; from the enumerant. Extensions have enumerants
  ; with string values, which will need to be handled
  ; separately.
  (define (extract-value enumerant)
    (if (attrs-have-key? enumerant 'alias)
        (extract-value (resolve-alias enumerant))
        (if (attrs-have-key? enumerant 'bitpos)
            (arithmetic-shift 1 (sub1 (string->number (attr-ref enumerant 'bitpos))))
            (let ([val (attr-ref enumerant 'value "999")])
              (when (equal? val "999")
                (displayln enumerant))
              (if (string-prefix? val "0x")
                  (string->number (string-replace val "0x" "") 16)
                  (string->number val))))))

  ; Pair up enumerant names and values.
  (define pairs (map (λ (x) (cons (attr-ref x 'name)
                                  (extract-value x)))
                     (filter (λ (x) (tag=? 'enum x))
                             (get-elements enum-decl))))

  ; To be nice to Racketeers, let's give them easy flags when
  ; using Vulkan so they don't have to OR things together themselves.
  (define ctype (if (equal? "bitmask" (attr-ref enum-decl 'type ""))
                    '_bitmask
                    '_enum))

  `(define ,(cname name)
     (,ctype
      ',(for/fold ([decls '()])
                  ([enumerant (in-list (reverse pairs))])
          ; The ctype declaration assumes a list of form (name0 = val0 name1 = val1 ...)
          (define w/value (cons (cdr enumerant) decls))
          (define w/= (cons '= w/value))
          (define w/all (cons (string->symbol (car enumerant)) w/=))
          w/all))))

(module+ test
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
    (check-equal?
     (generate-enum-signature '(type ((category "enum") (name "VkBlendOp")))
                              enum-registry)
     '(define _VkBlendOp
        (_enum '(VK_BLEND_OP_ADD = 0
                 VK_SHIMMED = 1
                 VK_BLEND_OP_SUBTRACT = 1
                 VK_BLEND_OP_REVERSE_SUBTRACT = 2
                 VK_BLEND_OP_MIN = 3
                 VK_BLEND_OP_MAX = 4))))
    (check-equal?
     (generate-enum-signature '(type ((category "enum") (name "NotPresent")))
                              enum-registry)
     '(define _NotPresent (_enum '())))

    (check-equal?
     (generate-enum-signature '(type ((category "enum") (name "VkShaderStageFlagBits")))
                              enum-registry)
     '(define _VkShaderStageFlagBits
        (_bitmask '(VK_SHADER_STAGE_VERTEX_BIT = 0
                    VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = 1
                    VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = 2
                    VK_SHADER_STAGE_GEOMETRY_BIT = 4
                    VK_SHADER_STAGE_FRAGMENT_BIT = 8
                    VK_SHADER_STAGE_COMPUTE_BIT = 16
                    VK_SHADER_STAGE_ALL_GRAPHICS = 31
                    VK_SHADER_STAGE_ALL = 2147483647))))))


; <type category="bitmask"> is just a C type declaration
; that happens to contain a typedef. Declaring _bitmask
; in Racket actually happens as part of processing enums.
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
  (define name (get-type-name funcpointer-xexpr))
  (define text-signature (get-all-cdata funcpointer-xexpr))
  
  ; Deduce the formal parameter types
  (define children (get-elements funcpointer-xexpr))
  (define parameter-type-elements (filter (λ (x) (tag=? 'type x)) children))
  (define adjacent-cdata (map (λ (type-xexpr)
                                (list-ref children
                                          (add1 (index-of children type-xexpr))))
                              parameter-type-elements))

  (define parameter-types (map (λ (type-xexpr decl)
                                 (infer-pointer-depth (shrink-wrap-cdata type-xexpr)
                                                      (string->list decl)))
                               parameter-type-elements
                               adjacent-cdata))
  
  ; Deduce the return type
  (define return-signature (cadr (regexp-match #px"typedef ([^\\(]+)" text-signature)))
  (define undecorated-return-type (regexp-replace* #px"[\\s\\*\\[\\]]" return-signature ""))
  (define return-type (infer-pointer-depth undecorated-return-type
                                           (string->list return-signature)))

  `(define ,(cname name) (_cpointer
                          (_fun ,@parameter-types
                                ->
                                ,return-type))))

(module+ test
  (test-equal? "(generate-funcpointer-signature)"
               (generate-funcpointer-signature
                '(type ((category "funcpointer"))
                       "typedef void* (VKAPI_PTR *" (name "PFN_vkAllocationFunction") ")(\n   "
                       (type "void") "* pUserData,"
                       (type "size_t") "size,"
                       (type "size_t") "alignment,"
                       (type "VkSystemAllocationScope") "allocationScope);"))
                '(define _PFN_vkAllocationFunction (_cpointer (_fun (_cpointer _void)
                                                                    _size_t
                                                                    _size_t
                                                                    _VkSystemAllocationScope
                                                                    ->
                                                                    (_cpointer _void))))))

(define (generate-signatures registry ordered)
  (define category=>proc
    `#hash(("ctype"       . ,generate-ctype-signature)
           ("basetype"    . ,generate-basetype-signature)
           ("symdecl"     . ,generate-symdecl-signature)
           ("define"      . ,generate-define-signature)
           ("handle"      . ,generate-handle-signature)
           ("enum"        . ,generate-enum-signature)
           ("bitmask"     . ,generate-bitmask-signature)
           ("funcpointer" . ,generate-funcpointer-signature)
           ("struct"      . ,generate-struct-signature)
           ("union"       . ,generate-union-signature)))

  (for/list ([type (in-list ordered)])
    (define category (attr-ref type 'category ""))
    (define alias (attr-ref type 'alias #f))
    (if alias
        `(define ,(cname (get-type-name type)) ,(cname alias))
        ((hash-ref category=>proc category) type registry))))

(module+ test
  (test-true "E2E does not crash when producing data."
             (andmap list?
                     (generate-vulkan-bindings (get-vulkan-spec 'local)))))
