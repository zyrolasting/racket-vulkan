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

(define (in-fragment registry [config #hash()])
  (in-generator
    (yield* (in-preamble registry config))
    (yield* (in-check-vkResult-signature registry config))
    (yield* (in-ctype-declarations registry config))
    (yield* (in-api-constant-declarations registry config))
    (yield* (in-typedef-declarations registry config))
    (yield* (in-relevant-preprocessor-declarations registry config))
    (yield* (in-handle-declarations registry config))
    (yield* (in-interdependent-declarations registry config))))
