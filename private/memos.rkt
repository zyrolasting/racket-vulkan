#lang racket/base

;; -------------------------------------------------------------
;; Responsible for caching the many expensive operations used to
;; gather information from the Vulkan spec.

(require racket/contract
         racket/dict)

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define/contract current-memo-cache
  (parameter/c dict?)
  (make-parameter (make-hash)))

(define (memoizer proc [key (gensym)])
  (λ A
    (define cache (current-memo-cache))
    (unless (hash-has-key? cache key)
      (hash-set! cache key (apply proc A)))
    (hash-ref cache key)))

(module+ test
  (test-case "Save return value from call"
    (define a-calls 0)
    (define a (memoizer (λ _
                          (set! a-calls (add1 a-calls))
                          a-calls)))
    (check-eq? (a) (a))
    (check-eqv? a-calls 1)
    (check-eq? (a 1) (a 1))
    (check-eqv? a-calls 1)
    (test-case "New cache, new calls"
      (parameterize ([current-memo-cache (make-hash)])
        (check-eq? (a 1) (a 1))
        (check-eqv? a-calls 2)))))
