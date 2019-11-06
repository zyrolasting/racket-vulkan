#lang racket/base

(module+ test
  (require rackunit
           "./defines.rkt")

  (test-equal? "(generate-define-signature)"
               (generate-define-signature '(type ((name "VkDeviceAddress"))))
               '(define _VkDeviceAddress 'VkDeviceAddress)))
