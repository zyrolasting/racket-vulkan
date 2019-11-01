#lang racket/base

(provide (all-defined-out))
(module+ test
  (require rackunit))

(define (memoizer proc)
  (define memos (make-hash))
  (λ A
    (if (hash-has-key? memos A)
        (hash-ref memos A)
        (let ([result (apply proc A)])
          (hash-set! memos A result)
          result))))

(module+ test
  (test-case "Can memoize based on equal?-comparable argument lists"
    (define a-calls 0)
    (define a (memoizer (λ _
                          (set! a-calls (add1 a-calls))
                          a-calls)))
    (check-eq? (a) (a))
    (check-eqv? a-calls 1)
    (check-eq? (a 1) (a 1))
    (check-eqv? a-calls 2)))
