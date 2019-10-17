#lang racket/base

;; -------------------------------------------------------------------
;; This module is responsible for curating the xexpr representation of
;; the Vulkan Registry into a new xexpr that's easier to process.

(provide (all-defined-out))

(require racket/function
         racket/list
         racket/string
         "./txexpr.rkt"
         "./c-analysis.rkt")

(define (get-types-by-category cat types)
  (filter (λ (x) (equal? (attr-ref x 'category "") cat))
          types))


;; The "define" <type> category includes both C macros and C type
;; declarations. Strip out the macros to disambiguate the data. Note
;; that the filter predicate assumes all macros start with VK_ and are
;; all caps. Just checking for '#define' in the text will remove
;; legitimate declarations.
(define (remove-c-macros types)
  (filter (λ (t)
            (define name (get-type-name t))
            (define category (attr-ref t 'category ""))
            (define is-macro (and (equal? category "define")
                                  (string-prefix? name "VK_")
                                  (equal? (string-upcase name)
                                          name)))
            (not is-macro))
          types))

;; Categorizes <type requires="vk_platform"> as "ctype".
;; Interestingly, they are not already of category "basetype".
(define (categorize-c-types types)
  ; Don't include types that ffi/unsafe already provides.
  (define (already-in-racket? x)
    (member (cname (get-type-name x))
            '(_void _float _double)))

  (map (λ (x)
         (define requires (attr-ref x 'requires ""))
         (if (and (equal? requires "vk_platform")
                  (not (already-in-racket? x)))
             (attr-set x 'category "ctype")
             x))
       types))

;; This clones names of free-floating, circular, or otherwise
;; hard-to-reach types into new types of a "symdecl" ("symbol
;; declare") category.  This is used "forward declare" types as mere
;; symbols and capture types from platform headers.
(define (categorize-forward-declarations types)
 (map (λ (x)
        (define requires (attr-ref x 'requires ""))
        (define category (attr-ref x 'category ""))
        (if (or (string-suffix? requires ".h")
                (member category '("struct" "union")))
            (attr-set x 'category "symdecl")
            x))
      types))


; Return types in sorted groups.
(define (curate-types registry)
  (define curate (compose categorize-forward-declarations
                          categorize-c-types
                          remove-c-macros))

  (define curated-types (curate (find-all-by-tag 'type registry)))

  (apply append
         (map (λ (c) (get-types-by-category c curated-types))
         '("ctype"
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
