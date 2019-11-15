#lang racket/base

(provide (all-defined-out))
(require racket/generator
         "../paths.rkt")

;; We embed unsafe-preamble.rkt directly so that clients
;; can generate low-level bindings that can operate
;; outside of the vulkan collection.
(define (in-fragment registry [config #hash()])
  (in-generator
   (call-with-input-file
     (build-path private-path "unsafe-preamble.rkt")
     (λ (in)
       (yield (read-line in)) ; Forward #lang line
       (let loop ([datum (read in)])
         (if (eof-object? datum)
             (void)
             (begin
               (yield datum)
               (loop (read in)))))))))
