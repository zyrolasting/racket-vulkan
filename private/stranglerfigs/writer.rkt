#lang racket/base

(provide (all-defined-out))
(require "./paths.rkt")

(define (write-sequence registry make-sequence [out (current-output-port)])
  (for ([declaration (make-sequence registry)])
    (writeln declaration out)))

(define (write-public-module! path-element registry make-sequence)
  (call-with-output-file #:exists 'replace
    (build-path package-path path-element)
    (λ (port)
      (displayln "#lang racket/base")
      (write-sequence registry make-sequence))))
