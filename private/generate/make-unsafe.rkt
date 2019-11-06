#lang racket/base

;---------------------------------------------------------------------------------------------------
; This module generates FFI bindings for Vulkan

(require racket/contract
         racket/list
         "../paths.rkt")

(provide
  (contract-out
    ; Return a list of datums that can be written as a Racket module.
    [generate-vulkan-bindings (-> vulkan-spec? sequence?)]))

(module+ main
  (write-package-module-file! (get-vulkan-spec 'local)
                              generate-vulkan-bindings
                              "unsafe.rkt"))

;;-----------------------------------------------------------------------------------
;; Implementation
;; Registry guide: https://www.khronos.org/registry/vulkan/specs/1.1/registry.html

(require racket/generator
         racket/hash
         racket/port
         racket/set
         racket/string
         "../../spec.rkt"        ; For sourcing VulkanAPI spec
         "../analyze/c.rkt"      ; For building predicates on C text.
         "../analyze/spec.rkt"   ; For making the registry easier to process.
         "../analyze/txexpr.rkt" ; For element analysis
         "../analyze/memos.rkt"  ; For memoization
         "../writer.rkt"         ; For writing to the file system
         "./basetypes.rkt"
         (only-in "./preamble.rkt" generate-preamble)
         (only-in "./api-constants.rkt" generate-api-constant-declarations)
         (only-in "./typedefs.rkt" generate-typedef-declarations)
         (only-in "./handles.rkt" generate-handle-declarations)
         (only-in "./defines.rkt" generate-relevant-preprocessor-declarations)
         "./shared.rkt")


(module+ test
  (require rackunit
           racket/list))

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

(define (generate-union-signature union-xexpr [registry #f] [lookup #hash()])
  (define members (get-elements-of-tag 'member union-xexpr))
  (define name (get-type-name union-xexpr))
  `(begin
     (define ,(cname name)
       (_union
        . ,(map generate-member-signature/union
                members)))
     . ,(map
         (λ (x ordinal)
           `(define (,(string->symbol (string-append name
                                                     "-"
                                                     (shrink-wrap-cdata (find-first-by-tag 'name x)))) u)
              (union-ref u ,ordinal)))
         members
         (range (length members)))))


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


;; ------------------------------------------------
;; C structs correspond to <type category="struct">

(define (generate-struct-signature struct-xexpr [registry #f] [lookup #hash()])
  (define struct-name (get-type-name struct-xexpr))
  (define (generate-member-signature member-xexpr)
    (define name (snatch-cdata 'name member-xexpr))
    (define enum (find-first-by-tag 'enum member-xexpr))
    (define numeric-length (regexp-match #px"\\[(\\d+)\\]" (shrink-wrap-cdata member-xexpr)))
    (define undecorated-type (snatch-cdata 'type member-xexpr))
    (define characters (string->list (shrink-wrap-cdata member-xexpr)))
    (define inferred-type (infer-type (if (equal? undecorated-type struct-name)
                                                  "void"
                                                  undecorated-type)
                                              characters
                                              lookup))

    (define type (if enum
                     `(_array ,inferred-type ,(string->symbol (shrink-wrap-cdata enum)))
                     (if numeric-length
                         `(_array ,inferred-type ,(string->number (cadr numeric-length)))
                         inferred-type)))

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


;; ------------------------------------------------------------------
;; Enumerations are related by <type> and <enums> elements. Some
;; <enums> elements are a list of #defines. Others are actual C enums.
;; This is the case that generates actual C enums.
(define collect-extensions-by-enum-name
  (memoizer (λ (registry)
                 (foldl (λ (extension result)
                          (foldl (λ (enumerant collecting-enumerants)
                                   (hash-set collecting-enumerants
                                             (attr-ref enumerant 'name)
                                             extension))
                                 result
                                 (or (find-all-by-tag 'enum extension)
                                     '())))
                        #hash()
                        (find-all-by-tag 'extension registry)))))


(define (collect-enumerants-by-name where)
  (foldl (λ (enum result)
           (if (attrs-have-key? enum 'name)
               (hash-set result (attr-ref enum 'name) enum)
               result))
         #hash()
         (find-all-by-tag 'enum where)))

(define collect-enumerants-by-name/all
  (memoizer collect-enumerants-by-name))
(define collect-enumerants-by-name/core
  (memoizer (λ (registry)
                 (collect-enumerants-by-name
                  (find-first-by-tag 'enums registry)))))
(define collect-enumerants-by-name/features
  (memoizer (λ (registry)
                 (define hashes (map collect-enumerants-by-name (find-all-by-tag 'feature registry)))
                 (if (empty? hashes)
                     #hash()
                     (apply hash-union hashes)))))

(define collect-enumerant-name-counts
  (memoizer (λ (registry)
                 (foldl (λ (enum result)
                          (if (attrs-have-key? enum 'name)
                              (hash-set result
                                        (attr-ref enum 'name)
                                        (add1 (hash-ref result (attr-ref enum 'name) 0)))
                              result))
                        #hash()
                        (find-all-by-tag 'enum registry)))))

(define collect-enumerant-relationships
  (memoizer
   (λ (registry)
     (foldl (λ (x res)
              (hash-set res
                        (attr-ref x 'extends)
                        (cons x (hash-ref res (attr-ref x 'extends) '()))))
            #hash()
            (or (findf*-txexpr
                 registry
                 (λ (x) (and (tag=? 'enum x)
                             (attrs-have-key? x 'extends))))
                '())))))


(define (generate-enum-signature enum-xexpr registry [lookup #hash()])
  (define name (get-type-name enum-xexpr))
  (define extension-lookup (collect-extensions-by-enum-name registry))
  (define enum-lookup (collect-enumerants-by-name/all registry))
  (define enum-lookup/core (collect-enumerants-by-name/core registry))
  (define enum-lookup/features (collect-enumerants-by-name/features registry))
  (define relationship-lookup (collect-enumerant-relationships registry))
  (define name-counts (collect-enumerant-name-counts registry))

  (define (belongs-to-extension? name)
    (hash-has-key? extension-lookup name))

  ; Some enumerants have values computed in terms of enum ranges in other extensions.
  ; The spec covers how to compute these values.
  ; https://www.khronos.org/registry/vulkan/specs/1.1/styleguide.html#_assigning_extension_token_values
  (define (find-extension-relative-value enumerant)
    ; In English: First try to get the "extnumber" attribute value on
    ; the enumerant. Failing that, find the <extension> element that
    ; has the enumerant as a descendent and grab its "number"
    ; attribute value
    (define ext-number
      (string->number
       (attr-ref enumerant 'extnumber
                 (λ _ (attr-ref
                       (hash-ref extension-lookup (attr-ref enumerant 'name))
                       'number)))))

    (define base-value 1000000000)
    (define range-size 1000)
    (define offset (string->number (attr-ref enumerant 'offset)))
    (+ base-value (* (- ext-number 1) range-size) offset))

  ; Empty enums are possible.
  ; https://github.com/KhronosGroup/Vulkan-Docs/issues/1060
  (define enum-decl (hash-ref (collect-named-enums registry)
                              name
                              (λ _ '(enums))))

  ; Some enumerants are an alias for another enumerant.
  (define (resolve-alias enumerant)
    (define alias (attr-ref enumerant 'alias))
    (attr-set
     (hash-ref enum-lookup alias)
     'name
     (attr-ref enumerant 'name)))

  ; Pull out the intended (assumed numerical) value
  ; from the enumerant.
  (define (extract-value enumerant)
    (if (attrs-have-key? enumerant 'alias)
        (extract-value (resolve-alias enumerant))
        (if (attrs-have-key? enumerant 'offset)
            (find-extension-relative-value enumerant)
            (let ([n (if (attrs-have-key? enumerant 'bitpos)
                         (arithmetic-shift 1 (string->number (attr-ref enumerant 'bitpos)))
                         (let ([val (attr-ref enumerant 'value)])
                           (if (string-prefix? val "0x")
                               (string->number (string-replace val "0x" "") 16)
                               (string->number val))))])
              (if (equal? "-" (attr-ref enumerant 'dir #f))
                  (* -1 n)
                  n)))))

  ; Find the enumerants that extend this type.
  (define extensions
    (hash-ref relationship-lookup
              name
              (λ _ '())))

  ; HACK: For now, ignore the extension enums that duplicate definitions.
  (define deduped
    (filter
     (λ (x)
       (<= (hash-ref name-counts (attr-ref x 'name) 0)
           1))
     extensions))

  (define enumerants (append
                      (filter (λ (x) (tag=? 'enum x))
                              (get-elements enum-decl))
                      deduped))

  ; Pair up enumerant names and values.
  (define pairs (reverse (map (λ (x) (cons (attr-ref x 'name)
                                           (extract-value x)))
                              enumerants)))

  ; To be nice to Racketeers, let's give them easy flags when
  ; using Vulkan so they don't have to OR things together themselves.
  (define ctype (if (equal? "bitmask" (attr-ref enum-decl 'type ""))
                    '_bitmask
                    '_enum))

  ; _enum or _bitmask need a basetype to match how the values are used.
  ; https://docs.racket-lang.org/foreign/Enumerations_and_Masks.html?q=_enum#%28def._%28%28lib._ffi%2Funsafe..rkt%29.__enum%29%29
  (define basetype
    (if (equal? ctype '_enum)
        (if (ormap (λ (pair) (< (cdr pair) 0)) pairs)
            '_fixint
            '_ufixint)
        '_uint))

  `(begin
     (define ,(cname name)
       (,ctype
        ',(for/fold ([decls '()])
                    ([enumerant (in-list pairs)])
            ; The ctype declaration assumes a list of form (name0 = val0 name1 = val1 ...)
            (define w/value (cons (cdr enumerant) decls))
            (define w/= (cons '= w/value))
            (define w/all (cons (string->symbol (car enumerant)) w/=))
            w/all)
        ,basetype))
     . ,(for/list ([enumerant (in-list (reverse pairs))])
          `(define ,(string->symbol (car enumerant)) ,(cdr enumerant)))))

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
        (define VK_SHADER_STAGE_ALL 2147483647)))))


;; ------------------------------------------------------------------
; <type category="bitmask"> is just a C type declaration that happens
; to contain a typedef. Declaring _bitmask in Racket actually happens
; as part of processing enums.

(define (generate-bitmask-signature bitmask-xexpr [registry #f] [lookup #hash()])
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


;; ------------------------------------------------------------------
;; <type category="funcpointer"> hurts a little because parameter
;; type tags are floating in a soup of C code. I assume that only
;; pointer indirection matters and check for '*' in the next sibling
;; strings after the parameter types. The return type is not even
;; in a tag at all, so I have a different approach to deduce it.

(define (generate-funcpointer-signature funcpointer-xexpr [registry #f] [lookup #hash()])
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
                                 (infer-type (shrink-wrap-cdata type-xexpr)
                                                     (string->list decl)
                                                     lookup))
                               parameter-type-elements
                               adjacent-cdata))

  ; Deduce the return type
  (define return-signature (cadr (regexp-match #px"typedef ([^\\(]+)" text-signature)))
  (define undecorated-return-type (regexp-replace* #px"[\\s\\*\\[\\]]" return-signature ""))
  (define return-type (infer-type undecorated-return-type
                                          (string->list return-signature)
                                          lookup))

  `(define ,(cname name)
     (_fun ,@parameter-types
           ->
           ,return-type)))

(module+ test
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
                                                        _pointer))))


;; ------------------------------------------------------------------
;; All that stuff above was just the data. Now let's talk functions.


;; The return value of a function in context of the FFI is a bit tricky.
;; We want to capture pass-by-reference values returned to Racket, and
;; incorporate return code checking. This procedure generates code for
;; use as a `maybe-wrapper` in the `_fun` form. This assumes that `r`
;; is the identifier bound to the function's normal return value.
(define (generate-maybe-wrapper vkResult? who)
  (if (not vkResult?)
      null
      `(-> (check-vkResult r ',who))))

(module+ test
  (test-case "(generate-maybe-wrapper)"
    (test-equal? "vkResult = #f"
                 (generate-maybe-wrapper #f 'w)
                 null)

    (test-equal? "vkResult = #t, many others"
                 (generate-maybe-wrapper #t 'w)
                 '(-> (check-vkResult r 'w)))))

(define (generate-type-spec param)
  (define c-code (shrink-wrap-cdata param))
  (define ctype/text (get-text-in-tagged-child 'type param))
  (define pointer? (string-contains? c-code "*"))

  (if pointer?
      '_pointer
      (cname ctype/text)))

(module+ test
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
                 '_pointer)))

(define (generate-command-signature command-xexpr [registry #f] [lookup #hash()])
  (define children (filter (λ (x) (and (txexpr? x)
                                       (member (get-tag x) '(param proto))))
                           (get-elements command-xexpr)))

  ; <proto> always comes first.
  ; https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#_contents_of_command_tags
  (define proto (car children))
  (define id (string->symbol (get-text-in-tagged-child 'name proto)))
  (define undecorated-return (get-text-in-tagged-child 'type proto))
  (define characters (string->list (shrink-wrap-cdata proto)))
  (define ret (infer-type undecorated-return
                                  characters
                                  lookup))

  (define param-elements (cdr children))
  (define type-specs (map generate-type-spec param-elements))

  `(define-vulkan ,id
     (_fun ,@type-specs
           ->
           ,(if (equal? ret '_void)
                ret
                `(r : ,ret))
           . ,(generate-maybe-wrapper (equal? undecorated-return "VkResult")
                                      id))))


(module+ test
  (test-equal? "(generate-command-signature)"
               (generate-command-signature
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
               '(define-vulkan vkCreateInstance
                  (_fun _pointer
                        _pointer
                        _pointer
                        -> (r : _VkResult)
                        -> (check-vkResult r 'vkCreateInstance)))))


(define (generate-check-vkResult-signature registry)
  (define with-success-codes
    (findf*-txexpr registry
                   (λ (x) (attrs-have-key? x 'successcodes))))

  (define possible-codes
    (for/fold ([working (set)])
              ([has-codes with-success-codes])
      (define comma-separated (attr-ref has-codes 'successcodes))
      (define success-code-strings (regexp-split #px"\\s*,\\s*"
                                                 (string-trim comma-separated)))
      (set-union working (apply set (map string->symbol success-code-strings)))))

  (list
     `(define -success-codes ',(set->list possible-codes))
     `(define (check-vkResult v who)
        (unless (member v -success-codes)
          (error who "failed: ~a" v)))))

(define (generate-interdependent-declarations registry)
  (in-generator
    (define ordered (curate-registry registry))
    (define lookup (get-type-lookup ordered))

    ; To be clear, this is a superset of the category attribute values
    ; you'd expect to find in the Vulkan registry. (curate-registry)
    ; introduced a few of its own, and they are not restricted to
    ; <type> elements.
    (define category=>proc
      `#hash(("enum"         . ,generate-enum-signature)
             ("bitmask"      . ,generate-bitmask-signature)
             ("funcpointer"  . ,generate-funcpointer-signature)
             ("struct"       . ,generate-struct-signature)
             ("union"        . ,generate-union-signature)
             ("command"      . ,generate-command-signature)))

    (for ([type (in-list ordered)])
      (define category (attr-ref type 'category ""))
      (define alias (attr-ref type 'alias #f))
      (define make-datum (hash-ref category=>proc category #f))
      (when make-datum
        (yield (if alias
                   (let ([namer (if (tag=? 'command type) string->symbol cname)])
                     `(define ,(namer (get-type-name type)) ,(namer alias)))
                   (make-datum type registry lookup)))))))

(define (generate-vulkan-bindings registry)
  (in-generator
    (yield* (generate-preamble))
    (yield* (generate-check-vkResult-signature registry))
    (yield* (generate-ctype-declarations registry))
    (yield* (generate-api-constant-declarations registry))
    (yield* (generate-typedef-declarations registry))
    (yield* (generate-relevant-preprocessor-declarations registry))
    (yield* (generate-handle-declarations registry))
    (yield* (generate-interdependent-declarations registry))))
