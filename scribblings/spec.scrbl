#lang scribble/manual
@require[@for-label[vulkan/spec
                    racket/base
                    racket/contract]]

@title{Controlling the Vulkan Specification}

@defmodule[vulkan/spec]

The @racketmodname[vulkan/spec] module provides data from
@litchar{vk.xml}, the machine-readable specification of Vulkan. It can
download the latest copy from the Khronos Group, and convert the
contents of any @litchar{vk.xml} to an X-expression.

@defproc[(vulkan-spec? [v any/c]) boolean?]{
Returns @racket[#t] if argument is a Vulkan specification according to
this collection, @racket[#f] otherwise.

Currently the validation is not always correct because it does not yet support
RelaxNG. This will come in a future version.
}

@defthing[vulkan-spec-sources/c (symbols 'local 'remote)]{
These symbols select which @litchar{vk.xml} to use where applicable.

@itemlist[
@item{@racket['local] references the mirror of @litchar{vk.xml} packaged in the @racketmodname[vulkan] collection. It may be out of date, but can be used offline.}
@item{@racket['remote] references the latest official copy of @litchar{vk.xml} published by the Khronos Group.}
]
}

@defproc[(get-vulkan-spec [spec-source vulkan-spec-sources/c 'local])
                          vulkan-spec?]{
Returns the machine-readable Vulkan specification from the given source.
}
