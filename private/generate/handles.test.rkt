#lang racket/base

(module+ test
  (require rackunit
           "./handles.rkt")

  (test-equal? "(generate-handle-signature)"
               (generate-handle-signature '(type ((category "handle"))
                                                 "MAKE_HANDLE(" (name "VkDevice") ")"))
               '(define _VkDevice (_cpointer/null 'VkDevice_T))))
