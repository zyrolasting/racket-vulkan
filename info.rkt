#lang info
(define collection "vulkan")
(define deps '("base" "txexpr" "graph-lib" "draw-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/vulkan.scrbl" (multi-page))))
(define pkg-desc "Vulkan integration for Racket")
(define version "0.0")
(define pkg-authors '("Sage Gerard"))
(define test-omit-paths '("unsafe.rkt"))
