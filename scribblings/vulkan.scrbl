#lang scribble/manual
@require[@for-label[racket/base]]

@title{Vulkan API Integration}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

This collection integrates Racket and the Vulkan API, with a focus on
low overhead and C code equivalency. With this collection, it is
possible to use Racket to follow along with Vulkan tutorials written
for C and C++.

@local-table-of-contents[]
@include-section{setup.scrbl}
@include-section{spec.scrbl}
@include-section{unsafe.scrbl}
@include-section{ravk.scrbl}
@include-section{maintainers.scrbl}