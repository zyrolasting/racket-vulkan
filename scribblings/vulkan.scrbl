#lang scribble/manual
@require[@for-label[racket/base]]

@title{Vulkan API Integration}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@defmodule[vulkan]

A Racket integration with the Vulkan API.

@bold{This project is incomplete, and has an unstable interface.} If you are interested in contributing
or making a suggestion, please contact me. The source is available @hyperlink["https://github.com/zyrolasting/racket-vulkan"]{here}.

@local-table-of-contents[]

@include-section{setup.scrbl}
@include-section{spec.scrbl}
@include-section{unsafe.scrbl}
