#lang racket/base

(provide (all-defined-out))
(module+ test
  (require rackunit))

(define (memoizer proc)
  (define val #f)
  (λ A
    (unless val
      (set! val (apply proc A)))
    val))

(module+ test
  (test-case "Save return value from call"
    (define a-calls 0)
    (define a (memoizer (λ _
                          (set! a-calls (add1 a-calls))
                          a-calls)))
    (check-eq? (a) (a))
    (check-eqv? a-calls 1)
    (check-eq? (a 1) (a 1))
    (check-eqv? a-calls 1)))
