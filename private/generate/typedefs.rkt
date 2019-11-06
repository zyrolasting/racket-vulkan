#lang racket/base

; --------------------------------------------------------------------
; This module generates Racket code from the <type category="basetype">
; elements in the Vulkan specification.

(provide (all-defined-out))
(require "./shared.rkt")

(define (in-fragment registry)
  (in-generator
   (for ([element (get-type-by-category "basetype" registry)])
     (yield (generate-basetype-signature element)))))

(define (generate-basetype-signature type-xexpr)
  (define name (get-type-name type-xexpr))
  (define original-type (shrink-wrap-cdata
                         (findf (λ (x) (tag=? 'type x))
                                (get-elements type-xexpr))))
  `(define ,(cname name) ,(cname original-type)))
