#lang racket/base

(provide (all-defined-out))
(require racket/set
         "./shared.rkt")

(define (in-fragment registry [config #hash()])
  (define with-success-codes
    (findf*-txexpr registry
                   (λ (x) (attrs-have-key? x 'successcodes))))

  (define possible-codes
    (for/fold ([working (set)])
              ([has-codes with-success-codes])
      (define comma-separated (attr-ref has-codes 'successcodes))
      (define success-code-strings (regexp-split #px"\\s*,\\s*"
                                                 (string-trim comma-separated)))
      (set-union working (apply set (map string->symbol success-code-strings)))))

  (list
     `(define -success-codes ',(set->list possible-codes))
     `(define (check-vkResult v who)
        (unless (member v -success-codes)
          (error who "failed: ~a" v)))))
