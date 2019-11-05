#lang racket/base

(provide (all-defined-out))
(require "./paths.rkt")

; This is how we deliver Racket code generated using the Vulkan spec.
; This procedure is kept seperate for the sake of a unit test.
(define (write-sequence registry make-sequence [out (current-output-port)])
  (for ([declaration (make-sequence registry)])
    (writeln declaration out)))


(define (write-racket-module-file! path registry make-sequence)
  (call-with-output-file #:exists 'replace
    path
    (λ (port)
      (displayln "#lang racket/base" port) ; Limits load time
      (write-sequence registry make-sequence port))))


; For maintainer use only.
(define (write-package-module-file! registry make-sequence . path-elements)
  (write-racket-module-file! (apply build-path package-path path-elements)
                             registry
                             make-sequence))
