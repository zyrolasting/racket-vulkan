#lang scribble/manual

@require[@for-label[racket/base]]

@title{@tt{ravk}: Vulkan Ecosystem Controls}

@tt{ravk} (pronounced "Ravick") is a command-line interface
that reports useful facts about a Vulkan integration in terms
of a @litchar{vk.xml} specification.

You can use @tt{ravk} to generate modules that operate
independently from this collection, generate Racket
code fragments for auditing purposes, and to review
information about the Vulkan specification itself.

Many commands operate on a copy of @tt{vk.xml}, and you
can specify which to use.

@verbatim[#:indent 4]|{
$ ravk ...           # Use local mirror of vk.xml
$ ravk --latest ...  # (or -l) Use latest vk.xml from the Khronos Group.
}|

@section{@tt{ravk show}: Review key information}

Use @tt{ravk show} to view helpful data about the code that
will run on the host system in an integration.

@subsection{@tt{ravk show spec}: Specification review}

It's critically important that all concerned parties
keep an eye on the Vulkan specification. You will need
information about the spec to know if you should upgrade
this collection or use a different version of @tt{vk.xml} to
generate code.

@verbatim[#:indent 4]|{
$ ravk show spec           # prints local mirror of vk.xml for offline use.
$ ravk show spec --xexpr   # (or -x) prints vk.xml as an X-expression. Ignored if -v is set.
$ ravk show spec --version # (or -v) Show specification version.
$ ravk -l show spec -x     # X-expression of latest vk.xml
}|

@bold{Maintainers:} The package mirror of @tt{vk.xml} should be periodically
updated by running @litchar{ravk show spec --latest > private/assets/vk.xml}.

@subsection{@tt{ravk show gend}: Access Generator Modules}

@tt{gend} (short for "generator directory") prints the location
of generator modules on your system.
@verbatim[#:indent 4]|{
$ ls `ravk show gend`
api-constants.rkt ...
}|

Be warned that this command invites access to code inside the
collection's installation on your system. The contents are subject
to change and should not be considered part of a stable API. However,
the modules that happen to be there are considered valid input to
@tt{ravk generate}. That command @italic{is} stable.

@section{@tt{ravk generate}}

@verbatim[#:indent 4]|{
$ ravk generate heading.rkt body.rkt footer.rkt ...
}|

The @tt{generate} command prints code to STDOUT. The output
may be a full Racket module body complete with a @litchar{#lang} line,
or a parseable fragment that follows unstated assumptions.

Use this command to inspect the Vulkan registry with
custom views, or to prepare code with an adjustable
level of overhead.

The arguments are paths to Racket modules that must provide
a procedure called @racket[in-fragment]. @racket[in-fragment]
must follow certain rules:

@itemlist[
@item{It accepts an X-expression of some @racket[vk.xml] and returns a sequence.}
@item{The first element of the sequence may be a string. If it is a string, it must be a complete @litchar{#lang} line. A trailing newline character is optional.}
@item{Every non-string element is a datum that, when printed in @racket[write] mode, is a valid Racket expression.}
]

The output of each @racket[in-fragment] procedure will appear in the
order declared in the command line. There is no general guarentee that
the total output will constitute a valid program, so you have to know
what you are asking for.

@subsection{Auditing Existing Generators}

To review the code this collection generates in whole or in part,
use one of the modules from @tt{ravk show gen}.

For example, this command prints all Vulkan API constants
as a sequence of Racket @racket[(define)] forms.

@verbatim[#:indent 4]|{
$ ravk generate "$(ravk show gend)/api-constants.rkt"
}|

If you run into a problem with the specification or the Racket
code produced from this collection, you or a maintainer can
leverage this tool to verify code fragments.


@subsection{Example: Your First Code Generator}

A code generator module lazily produces Racket code in terms
of a Vulkan specification. To write one, you need to
know the @hyperlink["https://www.khronos.org/registry/vulkan/specs/1.1/registry.html"]{Vulkan Registry}.

It so happens that code generator modules are easy to write with Racket
@secref["Generators" #:doc '(lib
"scribblings/reference/reference.scrbl")].  Be careful not to confuse
the two.

In this example, we generate a Racket module that prints platform-specific
type names when instantiated. @racket[registry-xexpr] is bound to an
X-expression form of the @tt{<registry>} (root) element of a Vulkan
specification.

@racketmod[#:file "gen-platform-type-printer.rkt"
racket/base

(provide in-fragment)
(require racket/generator
         racket/string
         txexpr)

(define (in-fragment registry-xexpr)
  (in-generator
    (yield "#lang racket/base")
    (define platform-type-elements
      (findf*-txexpr registry-xexpr
                     (λ (x)
                       (and (list? x)
                            (string-suffix? (attr-ref x 'requires "")
                                            ".h"))))
      (for ([element platform-type-elements])
        (yield `(displayln ,(attr-ref element 'name)))))))
]

For Vulkan 1.1.126, this session holds:

@verbatim[#:indent 4]|{
$ ravk generate gen-platform-type-printer.rkt > print-platform-types.rkt
$ racket print-platform-types.rkt
Display
VisualID
Window
RROutput
wl_display
wl_surface
HINSTANCE
HWND
HMONITOR
HANDLE
SECURITY_ATTRIBUTES
DWORD
LPCWSTR
xcb_connection_t
xcb_visualid_t
xcb_window_t
zx_handle_t
GgpStreamDescriptor
GgpFrameToken
}|

Code generators can also generate documentation or report data.
The possibilities are exciting, but some words of warning:

@itemlist[
@item{A search through the registry element is expensive. Use memoization and limit your search space whenever possible.}
@item{The Vulkan specification is machine-readable, but human-comprehensible. Expect to write weird and wonderful things to make your program work reliably. Mostly weird.}
@item{There are no guarentees that the order of data you encounter is the order it should appear in Racket.}
]

@section{@tt{ravk replicate}: Integrate Independently}

The @tt{replicate} command generates FFI bindings for Vulkan
with mininal protections (The output of this command is
the @racketmodname[vulkan/unsafe] module).

This effectively locks down a copy of Racket code
against a Vulkan spec version, and makes it possible
for some packages to operate without a dependency
on this collection.

@verbatim[#:indent 4]|{
$ ravk replicate > unsafe.rkt
}|
