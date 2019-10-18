#lang scribble/manual
@require[@for-label[xml
                    vulkan
                    racket/base
                    ffi/unsafe
                    ffi/unsafe/define]]

@title{Generating Vulkan API bindings}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@defmodule[vulkan]

A Racket integration with the Vulkan API.

To use this collection you must have the Vulkan dynamic library associated with your
platform installed---e.g. @litchar{libvulkan.so} for Linux or @litchar{vulkan1.dll} for Windows.
For development purposes this means installing the @hyperlink["https://vulkan.lunarg.com/sdk/home"]{Vulkan SDK}.
For consumption purposes this means installing graphic drivers including a copy the aforementioned library.

@bold{This project is nowhere @italic{near} complete.} If you are interested in contributing
or making a suggestion, please contact me. The source is available @hyperlink["https://github.com/zyrolasting/racket-vulkan"]{here}.

The interface should be considered unstable until declared otherwise.
My intention is to make all C function and type names in Vulkan are available verbatim,
so @hyperlink["https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateInstance.html"]{@litchar{vkCreateInstance}}
is available as a procedure called @litchar{vkCreateInstance}.
