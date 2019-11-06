#lang racket/base

;; ------------------------------------------------------------------
;; Generates Racket code to declare handle types. Handles are just
;; pointers to forward-declared structs with private definitions. We
;; use symbols to represent them on the Racket side.

(provide (all-defined-out))
(require "./shared.rkt")

(define (in-fragment registry)
  (in-generator
   (for ([element (get-type-by-category "handle" registry)])
     (yield (generate-handle-signature element)))))

(define (generate-handle-signature handle-xexpr)
  (define name (get-type-name handle-xexpr))
  `(define ,(cname name) (_cpointer/null ',(string->symbol (string-append name "_T")))))
