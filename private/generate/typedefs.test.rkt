#lang racket/base

(module+ test
  (require rackunit
           "./typedefs.rkt")

  (test-equal? "(generate-basetype-signature)"
               (generate-basetype-signature '(type ((category "basetype"))
                                                   "typedef "
                                                   (type "uint64_t")
                                                   " "
                                                   (name "VkDeviceAddress")
                                                   ";"))
               '(define _VkDeviceAddress _uint64_t)))
