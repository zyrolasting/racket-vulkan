#lang racket/base

(require racket/generator)
(provide (all-defined-out))

(define (yield* sequence)
  (for ([datum sequence])
    (yield datum)))
