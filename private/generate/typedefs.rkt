#lang racket/base

; --------------------------------------------------------------------
; This module generates Racket code from the <type category="basetype">
; elements in the Vulkan specification.


(provide (all-defined-out)
         (rename-out [generate-typedef-declarations generate-fragment]))

(require racket/generator
         racket/string
         "../analyze/spec.rkt"
         "../analyze/c.rkt"
         "../analyze/txexpr.rkt")

(define (generate-basetype-signature type-xexpr)
  (define name (get-type-name type-xexpr))
  (define original-type (shrink-wrap-cdata
                         (findf (λ (x) (tag=? 'type x))
                                (get-elements type-xexpr))))

  `(define ,(cname name) ,(cname original-type)))

(define (generate-typedef-declarations registry)
  (in-generator
   (for ([element (get-type-by-category "basetype" registry)])
     (yield (generate-basetype-signature element)))))
