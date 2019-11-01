#lang racket/base

(provide (all-defined-out))
(require racket/runtime-path)

(define-runtime-path private-path ".")
(define-runtime-path package-path "..")
(define-runtime-path assets-path "./assets")

