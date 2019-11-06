#lang racket/base

;; The "define" category may contain C code of several meanings for
;; our purposes. We cannot easily deal with the function-style C macros,
;; but we do want the ones that give us simple bindings. This module
;; generates those.

(provide (all-defined-out))
(require racket/string
         "./shared.rkt")

(define (in-fragment registry)
  (in-generator
   (for ([element (get-type-by-category "define" registry)])
     (unless (is-c-macro? element)
       (yield (generate-define-signature element))))))

(define (generate-define-signature type-xexpr)
  (define name (get-type-name type-xexpr))
  `(define ,(cname name) ',(string->symbol name)))

;; The "define" <type> category includes both C macros and C type
;; declarations presented as preprocessor directives. This predicate
;; should distinguish between the two.
(define (is-c-macro? type-xexpr)
  (define name (get-type-name type-xexpr))
  (define category (attr-ref type-xexpr 'category ""))
  (and (string-prefix? name "VK_")
       (equal? (string-upcase name)
               name)))
