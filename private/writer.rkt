#lang racket/base

(provide (all-defined-out))
(require racket/stream
         racket/string
         "./paths.rkt")

;; This is how we deliver Racket code generated using the Vulkan spec.
(define (write-sequence registry make-sequence [out (current-output-port)])
  (define seq (sequence->stream (make-sequence registry)))
  (define first-element (stream-first seq))

  (if (and (string? first-element) (string-prefix? first-element "#lang"))
      (displayln first-element out)
      (writeln first-element out))

  (for ([declaration (stream-rest seq)])
    (writeln declaration out)))

(module+ test
  (require racket/generator
           racket/port
           rackunit)

  (test-case "Write code fragment"
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
                 '((a) (b) (c))))

  (test-case "Write code module"
    (define-values (i o) (make-pipe))
    (write-sequence '(dummy)
                    (λ (x)
                      (in-generator (yield "#lang something") (yield '(b))))
                    o)
    (close-output-port o)
    (test-equal? "#lang line preserved"
                 (read-line i)
                 "#lang something")
    (test-equal? "Datum follows string"
                 (read i) '(b))))
