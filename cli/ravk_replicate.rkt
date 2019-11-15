#lang racket/base

(provide process-command-line summary)

(define summary "Generate raw and complete Vulkan FFI bindings")

(require racket/cmdline
         racket/runtime-path
         (only-in mzlib/etc this-expression-file-name)
         natural-cli
         "./shared.rkt"
         "../private/paths.rkt")

(define program-name (get-program-name (this-expression-file-name)))

(define (process-command-line)
  (command-line #:program program-name
                #:args _
                (write-generated (build-path private-path "generate/make-unsafe.rkt"))))
