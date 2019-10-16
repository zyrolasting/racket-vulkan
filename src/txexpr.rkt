#lang racket/base

;-----------------------------------------------------------------
; This module is for Tagged X-expression search patterns as they
; apply to vk.xml. This code just added noise when included in a
; module for assembling FFI signatures.

(provide (all-defined-out)
         (all-from-out txexpr))


;-----------------------------------------------------------------
; Implementation

(require racket/string
         racket/list
         txexpr)

(define (simple-memo proc)
  (define memo (void))
  (λ A
    (when (void? memo)
      (set! memo (apply proc A)))
    memo))


(define (with-attr name L)
  (filter (λ (x) (attrs-have-key? x name)) L))

(define (tag=? t tx)
  (and (txexpr? tx)
       (equal? (get-tag tx) t)))

(define (find-all-by-tag t tx)
  (findf*-txexpr tx (λ (x) (tag=? t x))))

(define (find-first-by-tag t tx)
  (findf-txexpr tx (λ (x) (tag=? t x))))

(define (shrink-wrap-cdata x)
  (string-trim (string-join (filter string? (get-elements x)) "")))

(define (snatch-cdata t tx #:children-only? [kidsonly #f])
  (shrink-wrap-cdata (find-first-by-tag t (if kidsonly (list-set tx 0 (gensym))
                                              tx))))

(define (get-elements-of-tag t tx)
  (filter (λ (x) (tag=? t x))
          (get-elements tx)))

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

(define collect-enums
  (simple-memo (λ (registry)
                  (find-all-by-tag 'enums registry))))

(define collect-named-enums
  (simple-memo (λ (registry)
                  (foldl (λ (x h) (if (attrs-have-key? x 'name)
                                      (hash-set h (attr-ref x 'name) x)
                                      h))
                         #hash()
                         (collect-enums registry)))))

(define (get-type-by-category cat registry)
    (findf*-txexpr registry
                   (λ (x) (and (txexpr? x)
                               (equal? 'type (get-tag x))
                               (equal? cat (attr-ref x 'category #f))))))
