#lang racket/base


(module+ test
  (require racket/sequence
           rackunit
           "./api-constants.rkt")

  (test-equal? "(find-extension-constants)"
               (find-extension-constants
                '(root (extension "\n    "
                                  (require (enum ((extends "A") (name "B")))
                                           (enum ((name "X") (value "1")))))))
               '(enums (enum ((name "X") (value "1")))))

  (test-equal? "(generate-consts-signature)"
               (sequence->list
                (generate-consts-signature
                 '(enums (enum ((value "(~0U)") (name "A")))
                         (enum ((value "(~0ULL-2)") (name "B")))
                         (enum ((value "(~0L)") (name "C")))
                         (enum ((value "256") (name "D")))
                         (enum ((value "(~0UL)") (dir "-1") (name "N")))
                         (enum ((name "E") (alias "C"))))))
               '((define A
                   (- (integer-bytes->integer (make-bytes (ctype-sizeof _long) 255) #t)
                      0))
                 (define B
                   (- (integer-bytes->integer (make-bytes (ctype-sizeof _llong) 255) #t)
                      2))
                 (define C
                   (- (integer-bytes->integer (make-bytes (ctype-sizeof _long) 255) #f)
                      0))
                 (define D 256)
                 (define N
                   (* -1
                      (- (integer-bytes->integer (make-bytes (ctype-sizeof _long) 255) #t)
                         0)))
                 (define E C))))
