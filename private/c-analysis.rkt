#lang racket/base

;; This module is responsible for making sense of strings containing
;; C code, without a parser. Ideally I would use a Clang plugin.

(provide (all-defined-out))

(require racket/list
         racket/string
         "./txexpr.rkt")

(module+ test
  (require rackunit))

(define (cname str)
  (string->symbol (string-append "_" str)))

(define (cnamef fmt-string . args)
  (cname (apply format fmt-string args)))

(define (infer-pointer-type undecorated-type characters [lookup #hash()])
  (if (and (hash-has-key? lookup undecorated-type)
           (category=? "struct" (hash-ref lookup undecorated-type)))
      (cname (string-append (let ([el (hash-ref lookup undecorated-type)])
                              (if (attrs-have-key? el 'alias)
                                  (attr-ref el 'alias)
                                  undecorated-type))
                            "-pointer/null"))
      (let ([pointer-depth (count (λ (ch) (char=? #\* ch)) characters)])
        ; Wrap pointer declarations equal to the number of '*'s
        (for/fold ([sig (cname undecorated-type)])
                  ([i (in-range pointer-depth)])
          (define ptrtype `(_cpointer/null ,sig))
          (if (member ptrtype '((_cpointer/null _int8) (_cpointer/null _char)))
              '_bytes/nul-terminated
              ptrtype)))))

(module+ test
  (test-equal? "No pointers"
               (infer-pointer-type "char" '())
               '_char)
  (test-equal? "Depth = 1"
               (infer-pointer-type "void" '(#\1 #\* #\f))
               '(_cpointer/null _void))
  (test-equal? "Depth = 2, mixed chars"
               (infer-pointer-type "int" '(#\1 #\* #\*))
               '(_cpointer/null (_cpointer/null _int)))
  (test-equal? "Special case: Struct name"
               (infer-pointer-type "ST" '() '#hash(("ST" . (type ((category "struct"))))))
               '_ST-pointer/null)
  (test-case "Special case: char*"
    (check-equal? (infer-pointer-type "char" '(#\*))
                  '_bytes/nul-terminated)
    (check-equal? (infer-pointer-type "char" '(#\* #\*))
                 '(_cpointer/null _bytes/nul-terminated))))
