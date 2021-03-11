#lang scribble/manual
@require[@for-label[setup/dirs
                    racket/base]]

@title{Setup}

Preparing this project for your system depends on what you want
to do. If you just want to use Vulkan for your program, follow the
first set of instructions. If you are a contributor
or want to use your copy of the source code, then
follow the second set of instructions.


@section{For Most Users}

@itemlist[#:style 'ordered
@item{Install this package using @litchar{raco pkg install vulkan}}
@item{Install the @hyperlink["https://www.lunarg.com/vulkan-sdk/"]{Vulkan SDK},
or the latest graphic drivers for your system with Vulkan support.}
@item{Confirm that @litchar{libvulkan.so} (Linux, etc.) or @litchar{vulkan-1.dll}
(Windows) are accessible via @racket[get-lib-search-dirs].}
]


@section{For Contributors and Power Users}

@itemlist[#:style 'ordered
@item{Clone https://github.com/zyrolasting/racket-vulkan/}
@item{Run @litchar{raco pkg remove vulkan} to remove any existing copy of this collection on your system.}
@item{In the new directory, run @litchar{raco pkg install --link} to install the collection as a link to your copy of the source.}
@item{Install the @hyperlink["https://www.lunarg.com/vulkan-sdk/"]{Vulkan SDK} or the latest graphic drivers for your system
with Vulkan support.}
@item{Confirm that @litchar{libvulkan.so} (Linux, etc.) or @litchar{vulkan-1.dll}
(Windows) are accessible via @racket[get-lib-search-dirs].}
@item{Run the unit tests using @litchar{raco test -x -p vulkan} to verify that the code is operational.}
@item{Run @litchar{racket examples/minimal.rkt} in the repository to make sure you can create and destroy a Vulkan instance.}]

As an aside, if you ever modify @litchar{unsafe.rkt}, be sure to run
@litchar{raco make} or @litchar{raco setup} on it to capture any changes.
It takes a while to compile.

