#lang racket/base

(provide (all-defined-out))
(require racket/runtime-path)

(define-runtime-path private-path ".")
(define-runtime-path package-path "..")
(define assets-path (build-path private-path "assets"))

