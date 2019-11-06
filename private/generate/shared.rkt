#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/generator
                       racket/string
                       "../memos.rkt"
                       "../c.rkt"
                       "../txexpr.rkt"))

(require racket/generator
         racket/string
         "../memos.rkt"
         "../c.rkt"
         "../txexpr.rkt")

(define (yield* sequence)
  (for ([datum sequence])
    (yield datum)))


(define get-type-elements
  (memoizer (λ (registry)
              (get-tagged-children (find-first-by-tag 'types registry)))))

(define get-type-lookup
  (memoizer
   (λ (types)
     (make-immutable-hash (map (λ (x) (cons (get-type-name x) x))
                               types)))))


; Type names appear in attribute or in CDATA of <name> element.
; https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#_attributes_of_type_tags
(define (get-type-name type-element)
  (attr-ref type-element
            'name
            (λ _
              (define name-element
                (findf-txexpr type-element
                              (λ (x) (and (list? x)
                                          (equal? (get-tag x) 'name)))))
              (and name-element
                   (shrink-wrap-cdata name-element)))))

(define (type-name=? type-element name)
  (equal? (get-type-name type-element) name))

(define (get-type-by-category cat registry)
    (findf*-txexpr registry
                   (λ (x) (and (txexpr? x)
                               (equal? 'type (get-tag x))
                               (equal? cat (attr-ref x 'category #f))))))
