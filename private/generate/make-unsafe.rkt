#lang racket/base

;; ----------------------------------------------------------------
;; Generates a Racket module exposing the raw C API for each Vulkan
;; version across all extensions and all platforms.

(provide (all-defined-out))
(require racket/generator
         "./shared.rkt"
         (only-in "./vkresult-checker.rkt" generate-check-vkResult-signature)
         (only-in "./basetypes.rkt" generate-ctype-declarations)
         (only-in "./preamble.rkt" generate-preamble)
         (only-in "./api-constants.rkt" generate-api-constant-declarations)
         (only-in "./typedefs.rkt" generate-typedef-declarations)
         (only-in "./handles.rkt" generate-handle-declarations)
         (only-in "./defines.rkt" generate-relevant-preprocessor-declarations)
         (only-in "./interdependent.rkt" generate-interdependent-declarations))

(module+ main
  (require "../../spec.rkt"
           "../writer.rkt")
  (write-package-module-file! (get-vulkan-spec 'local)
                              generate-vulkan-bindings
                              "unsafe.rkt"))

(define (generate-vulkan-bindings registry)
  (in-generator
    (yield* (generate-preamble))
    (yield* (generate-check-vkResult-signature registry))
    (yield* (generate-ctype-declarations registry))
    (yield* (generate-api-constant-declarations registry))
    (yield* (generate-typedef-declarations registry))
    (yield* (generate-relevant-preprocessor-declarations registry))
    (yield* (generate-handle-declarations registry))
    (yield* (generate-interdependent-declarations registry))))
