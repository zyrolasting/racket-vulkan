#lang racket/base

;; This module is responsible for making sense of strings containing
;; C code, without a parser. Ideally I would use a Clang plugin.

(provide (all-defined-out))

(require racket/list
         racket/string)

(module+ test
  (require rackunit))

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
    (define ptrtype `(_cpointer/null ,sig))
    (if (member ptrtype '((_cpointer/null _int8) (_cpointer/null _char)))
        '_bytes/nul-terminated
        ptrtype)))

(module+ test
  (test-equal? "No pointers"
               (infer-pointer-depth "char" '())
               '_char)
  (test-equal? "Depth = 1"
               (infer-pointer-depth "void" '(#\1 #\* #\f))
               '(_cpointer/null _void))
  (test-equal? "Depth = 2, mixed chars"
               (infer-pointer-depth "int" '(#\1 #\* #\[))
               '(_cpointer/null (_cpointer/null _int)))
  (test-case "Special case: char*"
    (check-equal? (infer-pointer-depth "char" '(#\*))
                  '_bytes/nul-terminated)
    (check-equal? (infer-pointer-depth "char" '(#\* #\*))
                 '(_cpointer/null _bytes/nul-terminated))))
