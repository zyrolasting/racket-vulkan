#lang info
(define collection "racket-vulkan")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/racket-vulkan.scrbl" ())))
(define pkg-desc "Generate Racket FFI bindings from Vulkan API registry.")
(define version "0.0")
(define pkg-authors '(sage))
