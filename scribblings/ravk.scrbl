#lang scribble/manual

@require[@for-label[racket/base racket/dict ffi/unsafe xml vulkan/spec]]

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
$ ravk -l show spec -x     # X-expression of latest vk.xml}|

@section{@tt{ravk generate}: Generate Code to use Vulkan}

@verbatim[#:indent 4]|{
$ ravk generate heading.rkt body.rkt footer.rkt ...
}|

The @tt{generate} command prints code to STDOUT. The output
may be a full Racket module body complete with a @litchar{#lang} line,
or a parseable fragment that follows certain assumptions.

Use this command to inspect the Vulkan registry with
custom views, or to prepare code with an adjustable
level of overhead.

The arguments are paths to Racket modules that must provide a
procedure called @racket[in-fragment]. It returns a sequence of Racket
code given a Vulkan specification and a shared configuration.

@racketblock[
(require racket/generator)

(define (in-fragment registry [config #hash()])
  (in-generator
    (yield "#lang racket/base")
    (yield '(define something 1))))]

If the first element of the sequence is a string, it will be printed in @racket[display]
mode. This is to support @litchar{#lang} lines. In this case, a trailing newline character
is optional. In all other cases, each element of the sequence is a datum that, when printed
in @racket[write] mode, will appear in a file as a valid Racket expression.

@racket[config] is populated by the @tt{ravk generate} command such that the long form
name of each option is a key in the dictionary (e.g. @litchar{--enable-auto-check-vkresult}
has the key @racket['enable-auto-check-vkresult]. Keys might not be present, and the value
of each key depends on the option. See @secref{genconfig}.

The output of each module appears follows the order declared in the
command line. There is no guarentee that the output will be a valid
program, so you have to know what you are asking for.

@subsection{Using Built-in Generators}

Besides paths to Racket modules, you can symbolically refer
to built-in generators. In addition, the output of
@litchar{ravk generate unsafe} in particular is the
content of @racketmodname[vulkan/unsafe].

This effectively locks down a copy of Racket code against a Vulkan
spec version, and makes it possible for some packages to operate
without a dependency on this collection, and with minimal overhead.

@verbatim[#:indent 4]|{
$ ravk generate unsafe > unsafe.rkt
}|

Here are the currently supported built-in generators:

@tabular[
#:style 'boxed
#:column-properties '(left right)
'(("unsafe" "FFI bindings for Vulkan across all platforms and extensions for the given specification."))]

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

(define (in-fragment registry-xexpr [config #hash()])
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

@subsection[#:tag "genconfig"]{Generator Configuration Space}

The built-in code generators share configuration that controls
their output. You can control this configuration using command-line
flags.

@subsubsection{Switches}

The following values are booleans for @racket[in-fragment]'s purposes.
If the flag is not set, the key will not be set in the dictionary
passed to @racket[in-fragment].

@itemlist[
@item{@litchar{--enable-auto-check-vkresult}: When set, all foreign
function calls that return a @tt{VkResult} will be automatically
checked in a wrapping Racket procedure. If the code is an error code,
the wrapping procedure will raise @racket[exn:fail].}
@item{@litchar{--enable-symbolic-enums}: When set, all C enum types
are represented using either @racket[_enum] or @racket[_bitmask]
depending on their intended use. You must then use symbols to represent
enumerants according to the rules of @racket[_enum] or @racket[_bitmask]
in your program.}
]
