#lang racket/base

;; Tagged X-expression procedures adapted to this project.

(provide (all-defined-out)
         (all-from-out txexpr))

(require (only-in racket/string string-trim string-join)
         txexpr)

(define (with-attr name L)
  (filter (λ (x) (attrs-have-key? x name)) L))

(define (tag=? t tx)
  (and (txexpr? tx)
       (equal? (get-tag tx) t)))

(define (category=? c tx)
  (and (txexpr? tx)
       (equal? (attr-ref tx 'category #f) c)))

(define (find-all-by-tag t tx)
  (or (findf*-txexpr tx (λ (x) (tag=? t x)))
      '()))

(define (find-first-by-tag t tx)
  (findf-txexpr tx (λ (x) (tag=? t x))))

(define (get-tagged-children tx)
  (filter txexpr? (get-elements tx)))

(define (get-types-by-category cat types)
  (filter (λ (x) (equal? (attr-ref x 'category "") cat))
          types))

(define (shrink-wrap-cdata x)
  (string-trim (string-join (filter string? (get-elements x)) "")))

(define (get-text-in-tagged-child t tx)
  (shrink-wrap-cdata (find-first-by-tag t tx)))

(define (get-all-cdata x)
  (foldl (λ (kid str)
           (string-append str
                          (if (string? kid)
                              kid
                              (get-all-cdata kid))))
         ""
         (get-elements x)))

(define (snatch-cdata t tx #:children-only? [kidsonly #f])
  (shrink-wrap-cdata (find-first-by-tag t (if kidsonly (cons (gensym) (cdr tx))
                                              tx))))

(define (get-elements-of-tag t tx)
  (filter (λ (x) (tag=? t x))
          (get-elements tx)))
