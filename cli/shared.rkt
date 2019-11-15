#lang racket/base

(provide (all-defined-out))
(require "../private/writer.rkt"
         "../spec.rkt")

(define use-latest? (make-parameter #f))

(define (write-generated modpath)
  (define source (if (use-latest?) 'remote 'local))
  (write-sequence (get-vulkan-spec source)
                  (dynamic-require modpath
                                   'in-fragment)))
