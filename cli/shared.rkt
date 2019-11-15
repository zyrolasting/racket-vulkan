#lang racket/base

(provide (all-defined-out))
(require "../private/writer.rkt"
         "../spec.rkt")

(define use-latest? (make-parameter #f))
(define (get-source/dynamic) (if (use-latest?) 'remote 'local))
(define (get-vulkan-spec/dynamic) (get-vulkan-spec (get-source/dynamic)))

(define (write-generated modpath)
  (write-sequence (get-vulkan-spec/dynamic)
                  (dynamic-require (if (string? modpath)
                                       (string->path modpath)
                                       modpath)
                                   'in-fragment)))
