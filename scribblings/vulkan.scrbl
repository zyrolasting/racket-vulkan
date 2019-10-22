#lang scribble/manual
@require[@for-label[vulkan
                    setup/dirs
                    racket/base
                    ffi/unsafe
                    ffi/unsafe/define]]

@title{Generating Vulkan API bindings}
@author[(author+email "Sage L. Gerard" "sage@sagegerard.com" #:obfuscate? #t)]

@defmodule[vulkan]

A Racket integration with the Vulkan API.

@bold{This project is incomplete, and has an unstable interface.} If you are interested in contributing
or making a suggestion, please contact me. The source is available @hyperlink["https://github.com/zyrolasting/racket-vulkan"]{here}.

@section{Setup}

@itemlist[#:style 'ordered
@item{Install the @hyperlink["https://www.lunarg.com/vulkan-sdk/"]{Vulkan SDK},
or the latest graphic drivers compatible with Vulkan. Before proceeding,
confirm that @litchar{libvulkan.so} (Linux) or @litchar{vulkan1.dll}
(Windows) are accessible via @racket[get-lib-search-dirs].}

@item{Either install this package using @tt{raco pkg install vulkan},
or clone the repository and install it locally as a package.}
]

@section{Unsafe Bindings}
@defmodule[vulkan/unsafe]

The unsafe module is exactly as the name implies. Use at your
own risk.

It provides over 1300 bindings representing all supported and
deprecated Vulkan structs, unions, enumerated types, constants and
functions across all platforms, extensions, and published Vulkan API
verions. There are few protections against misuse. This means that
@bold{mistakes risk undefined behavior and memory leaks, making this
module a poor choice for anyone seeking clear error messages,
strong validation, and concise code.}

The module is designed such that you can closely mirror an equivalent
C program. This makes it easier to port existing Vulkan programs to
Racket.  You should only use this module to write your own
abstractions in terms of @racketmodname[ffi/unsafe], or to study
Vulkan via Racket while following along with tutorials or
documentation written for C or C++.

It is possible to write cross-platform and backwards-compatible Vulkan
applications with this module, but you are responsible for detecting
the platform and using APIs from the correct version. This module is
simply a dump of all things Vulkan into Racket. So, when you
@racket[require] this module, you may assume that all Vulkan
identifiers defined in the specification for the core API and all
extensions are provided in your dependent module's namespace.

The translation from C to Racket is not 1-to-1. Observe the following caveats:

@margin-note{To repeat, the interface is unstable and these caveats are subject to change.}
@itemlist[
@item{All identifiers acting as C types are prefixed with an underscore. So, the C type @tt{VkInstance} is bound to the @tt{_VkInstance} identifier in Racket.}
@item{All enumerants are symbols. e.g. @tt{VK_SUCCESS} is @racket['VK_SUCCESS] in Racket. Take care to use symbols for purposes of comparing return codes, or passing arguments to C functions.}
@item{All enumerants acting as bitmasks are declared using @racket[_bitmask], and should be passed
to functions in a list without use of @racket[bitwise-ior] on your end.}
@item{API constants that are NOT from enumerated types are identifiers bound to a Racket value. e.g. @tt{VK_API_VERSION_1_1}.}
@item{All Vulkan functions are provided as Racket procedures with an identifier matching the name. e.g. The @tt{vkCreateInstance} C function is a Racket procedure also named @tt{vkCreateInstance}.}
@item{A Racket procedure's presence is not a guarentee that the associated C function is available as an object in the system library. If you call @tt{vkCreateAndroidSurfaceKHR} on a non-Android platform, the C function will not be found.}
@item{Unions are generated with accessor procedures that wrap @racket[union-ref]. So if a union @tt{U} has a member named @tt{floats}, you can access all Racket values converted from that union using @tt{(U-floats union-val)}.}
@item{Structs are created using @racket[define-cstruct], meaning that all bindings generated from that form exist for every Vulkan structure (e.g. @tt{_VkImageCreateInfo-pointer/null}).}
@item{As an aid, all functions that return a @tt{VkResult} are automatically
checked. Any return value that is not @tt{VK_SUCCESS} translates to a raised
@racket[exn:fail].}
]
