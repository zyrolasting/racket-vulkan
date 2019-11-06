#lang racket/base

;; Generates a Racket module exposing the raw C API for each Vulkan
;; version across all extensions and all platforms.

(provide (all-defined-out))
(require (for-syntax racket/base)
         racket/generator
         "./shared.rkt")

(define-syntax-rule (require-fg path id)
  (require (rename-in path [in-fragment id])))

(require-fg "./vkresult-checker.rkt" in-check-vkResult-signature)
(require-fg "./ctypes.rkt" in-ctype-declarations)
(require-fg "./preamble.rkt" in-preamble)
(require-fg "./api-constants.rkt" in-api-constant-declarations)
(require-fg "./typedefs.rkt" in-typedef-declarations)
(require-fg "./handles.rkt" in-handle-declarations)
(require-fg "./defines.rkt" in-relevant-preprocessor-declarations)
(require-fg "./interdependent.rkt" in-interdependent-declarations)

(define (in-fragment registry)
  (in-generator
    (yield* (in-preamble))
    (yield* (in-check-vkResult-signature registry))
    (yield* (in-ctype-declarations registry))
    (yield* (in-api-constant-declarations registry))
    (yield* (in-typedef-declarations registry))
    (yield* (in-relevant-preprocessor-declarations registry))
    (yield* (in-handle-declarations registry))
    (yield* (in-interdependent-declarations registry))))
