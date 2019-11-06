#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/generator
                       racket/string
                       "../analyze/memos.rkt"
                       "../analyze/spec.rkt"
                       "../analyze/c.rkt"
                       "../analyze/txexpr.rkt"))

(require racket/generator
         racket/string
         "../analyze/memos.rkt"
         "../analyze/spec.rkt"
         "../analyze/c.rkt"
         "../analyze/txexpr.rkt")

(define (yield* sequence)
  (for ([datum sequence])
    (yield datum)))
