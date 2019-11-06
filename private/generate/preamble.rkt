#lang racket/base

(provide generate-preamble
         (rename-out [generate-preamble generate-fragment]))

(require racket/generator
         "../paths.rkt")

; We embed unsafe-preamble.rkt directly so that clients
; can generate low-level bindings that can operate
; outside of the vulkan collection.
(define (generate-preamble)
  (in-generator
   (call-with-input-file
     (build-path private-path "unsafe-preamble.rkt")
     (λ (in)
       (read-line in) ; discard #lang line
       (let loop ([datum (read in)])
         (if (eof-object? datum)
             (void)
             (begin
               (yield datum)
               (loop (read in)))))))))
