#lang racket/base

(module+ test
  (require rackunit
           "basetypes.rkt")
  (test-equal? "Generate ctype without _t"
               (generate-ctype-signature '(type ((category "ctype") (name "void"))))
               '(define _void _void))
  (test-equal? "Generate ctype signature with _t"
               (generate-ctype-signature '(type ((category "ctype") (name "uint32_t"))))
               '(define _uint32_t _uint32)))
