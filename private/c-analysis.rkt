#lang racket/base

;; This module is responsible for making sense of strings containing
;; C code, without a parser. Ideally I would use a Clang plugin.

(provide (all-defined-out))

(require racket/list
         racket/string)

(define (cname str)
  (string->symbol (string-append "_" str)))

(define (cnamef fmt-string . args)
  (cname (apply format fmt-string args)))

(define (infer-pointer-depth undecorated-type characters)
  (define pointer-depth
    (count (λ (ch) (or (char=? #\* ch)
                       (char=? #\[ ch))) ; TODO: Should this be wrapped as an array type?
           characters))

  ; Wrap pointer declarations equal to the number of '*'s
  (for/fold ([sig (cname undecorated-type)])
            ([i (in-range pointer-depth)])
    `(_cpointer ,sig)))
