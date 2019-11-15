#lang racket/base

; -------------------------------------------------------------------
; This module generates the most basic, platform-agnostic C type
; declarations that appear in vk.xml.

(provide (all-defined-out))
(require "./shared.rkt")

(define (in-fragment registry [config #hash()])
  (in-generator
   (for ([declaration (find-ctype-declarations registry)])
     (yield (generate-ctype-signature declaration)))))


; The keys are C type names as they appear in vk.xml.  The values are
; what the identifiers should be in Racket, without the leading
; underscore.
(define name=>existing
  #hash(("char"      . "sbyte")
        ("void"      . "void")
        ("uint32_t"  . "uint32")
        ("float"     . "float")
        ("double"    . "double")
        ("uint8_t"   . "uint8")
        ("uint16_t"  . "uint16")
        ("uint32_t"  . "uint32")
        ("uint64_t"  . "uint64")
        ("int32_t"   . "int32")
        ("int64_t"   . "int64")
        ("size_t"    . "size")))


(define (find-ctype-declarations registry)
  (define (already-in-racket? x)
    (member (cname (get-type-name x))
            '(_void _float _double _int)))

  ; <type requires="vk_platform"> designates C types.
  ; Keep only those that ffi/unsafe does not already
  ; provide.
  (filter
   (λ (x)
     (and (equal? (attr-ref x 'requires "") "vk_platform")
          (not (already-in-racket? x))))
   (get-type-elements registry)))


(define (generate-ctype-signature type-xexpr)
  (define registry-type-name (get-type-name type-xexpr))
  (define racket-id (cname registry-type-name))
  (define type-id
   (if (hash-has-key? name=>existing registry-type-name)
       (hash-ref name=>existing registry-type-name)
       ; The _t replacement occurs because Racket C numeric types exclude them,
       ; and Racket already has bindings for _size, _uint8, etc.
       (string-replace registry-type-name
                       "_t"
                       "")))

  `(define ,racket-id ,(cname type-id)))
