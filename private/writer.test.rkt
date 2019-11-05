#lang racket/base

(module+ test
  (require racket/generator
           racket/port
           rackunit
           "./writer.rkt")

  (test-case "Generator-port integration"
    (define-values (i o) (make-pipe))
    (define registry '(dummy))
    (write-sequence registry
                    (λ (x)
                      (test-eq? "Registry referenced passed" registry x)
                      (in-generator (yield '(a)) (yield '(b)) (yield '(c))))
                    o)
    (close-output-port o)
    (test-equal? "Read values match order from generator"
                 (port->list read i)
                 '((a) (b) (c)))))
