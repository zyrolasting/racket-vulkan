#lang racket/base

;; ------------------------------------------------------------------
;; Generates Racket code to declare handle types. Handles are just
;; pointers to forward-declared structs with private definitions. We
;; use symbols to represent them on the Racket side.

(provide (all-defined-out))
(require racket/generator
         racket/string
         "../analyze/spec.rkt"
         "../analyze/c.rkt"
         "../analyze/txexpr.rkt")

(define (generate-handle-signature handle-xexpr [registry #f] [lookup #hash()])
  (define name (get-type-name handle-xexpr))
  `(define ,(cname name) (_cpointer/null ',(string->symbol (string-append name "_T")))))

(define (generate-handle-declarations registry)
  (in-generator
   (for ([element (get-type-by-category "handle" registry)])
     (yield (generate-handle-signature element)))))
