#lang racket/base

; --------------------------------------------------------------------
; This module generates Racket code from the <type category="basetype">
; elements in the Vulkan specification.

(provide (all-defined-out))
(require "./shared.rkt")

(define (in-fragment registry [config #hash()])
  (in-generator
   (for ([element (get-type-by-category "basetype" registry)])
     (yield (generate-basetype-signature element)))))

(define (generate-basetype-signature type-xexpr)
  (define name (get-type-name type-xexpr))
  (define maybe-type-element (findf (λ (x) (tag=? 'type x)) (get-elements type-xexpr)))
  (define original-type (if maybe-type-element (shrink-wrap-cdata maybe-type-element) name))
  `(define ,(cname name)
     ,(if (equal? name original-type)
          `',(string->symbol name)
          (cname original-type))))
