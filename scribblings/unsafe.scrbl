#lang scribble/manual
@require[@for-label[racket/base
                    racket/contract
                    @except-in[ffi/unsafe ->]
                    ffi/unsafe/define
                    vulkan/unsafe]
                    ffi/unsafe]

@title{Unsafe Bindings}
@defmodule[vulkan/unsafe]

The unsafe module is exactly as the name implies.
@bold{Use at your own risk.}

@racketmodname[vulkan/unsafe] provides over 2200 bindings representing
all supported and deprecated Vulkan structs, unions, enumerated types,
constants and functions across all platforms, extensions, and
published Vulkan API verions. There are few protections against
misuse. @italic{Any mistakes risk undefined behavior, race
conditions, and memory leaks}. Do not expect clear error messages,
strong validation, or concise code.

Here you will also find platform-specific code,
necessary loading calls to communicate with Vulkan,
and a curated list of utilities not generated from a specification.

On instantiation, this module will attempt to load the host
platform's Vulkan library. Should this fail, the module will
raise an error.


@section{Design Considerations}

Since @racketmodname[vulkan/unsafe] exposes a raw API, you can easily
port a C program using Vulkan to Racket. You can also use this module
to write your own abstractions in terms of @racketmodname[ffi/unsafe],
or to study established Vulkan literature while using Racket.

It is possible to write cross-platform and backwards-compatible Vulkan
applications with this module, but you are responsible for detecting
the platform and using APIs from the correct version.


@section{What Did I Just Load?}

When you @racket[require] this module, you may assume that all Vulkan
identifiers defined in the specification for the core API and all
extensions are provided. I cannot document every binding by hand,
but I thankfully don't have to. The translation from C to Racket is
not 1-to-1, but it is close enough where you can compare the below
list of translation rules against a Vulkan specification document
to know what bindings are available:

@margin-note{To repeat, the interface is unstable and these items are subject to change.}
@itemlist[
@item{All identifiers acting as C types are prefixed with an underscore. So, the C type @tt{VkInstance} is bound to the @tt{_VkInstance} identifier in Racket.}
@item{All enumerants are available as interned symbols and identifiers. The Racket identifier @tt{VK_SUCCESS} binds to the Racket value equivalent of @tt{VK_SUCCESS} In Vulkan. However, symbols like @racket['VK_SUCCESS] are available for FFI wrapper procedures that translate @racket[_enum] or @racket[_bitmask] values.}
@item{API constants that are NOT from enumerated types are identifiers bound to a Racket value. e.g. @tt{VK_API_VERSION_1_1}.}
@item{All Vulkan functions are provided as Racket procedures with an identifier matching the name. e.g. The @tt{vkCreateInstance} C function is a Racket procedure also named @tt{vkCreateInstance}.}
@item{A Racket procedure's presence is not a guarentee that the associated C function is available as an object in the system library. If you call @tt{vkCreateAndroidSurfaceKHR} on a non-Android platform, the C function will not be found.}
@item{Unions are generated with accessor procedures that wrap @racket[union-ref]. So if a union @tt{U} has a member named @tt{floats}, you can access all Racket values converted from that union using @tt{(U-floats union-val)}.}
@item{Structs are created using @racket[define-cstruct], meaning that all bindings generated from that form exist for every Vulkan structure (e.g. @tt{_VkImageCreateInfo-pointer/null}).}
@item{As an aid, all functions that return a @tt{VkResult} are automatically
checked. Any return value that is not @tt{VK_SUCCESS} translates to a raised
@racket[exn:fail].}
]

@section{Platform-Specific Definitions}

Vulkan is itself platform-agnostic, but extensions are necessary to
integrate with platforms. Some identifiers are bound to interned
symbols--such as @racket[_Display]-- because they are meant for use in
@racket[_cpointer] or @racket[_cpointer/null].

@subsection{X + RandR}
@deftogether[(
@defthing[_VisualID ctype? #:value _ulong]
@defthing[_Window ctype? #:value _ulong]
@defthing[_RROutput ctype? #:value _ulong]
@defthing[_Display symbol? #:value 'Display]
)]{}

@subsection{Wayland}
@deftogether[(
@defthing[_wl_display symbol? #:value 'wl_display]
@defthing[_wl_surface symbol? #:value 'wl_surface]
)]{}

@subsection{Windows}
@deftogether[(
@defthing[_HANDLE ctype? #:value (_cpointer _void)]
@defthing[_HINSTANCE ctype? #:value _HANDLE]
@defthing[_HWND  ctype? #:value _HANDLE]
@defthing[_HMONITOR ctype? #:value _HANDLE]
@defthing[_DWORD ctype? #:value _ulong]
@defthing[_LPCWSTR  ctype? #:value (_cpointer _wchar)]
@defthing[_SECURITY_ATTRIBUTES symbol? #:value 'SECURITY_ATTRIBUTES]
)]{}

@subsection{XCB}
@deftogether[(
@defthing[_xcb_visualid_t  ctype? #:value _uint32]
@defthing[_xcb_window_t  ctype? #:value _uint32]
@defthing[_xcb_connection_t symbol? #:value 'xcb_connection_t]
)]{}

@subsection{Zircon (Fuchsia OS)}
@defthing[_zx_handle_t ctype? #:value _uint32]{}

@subsection{Google Games}
@deftogether[(
@defthing[_GgpStreamDescriptor ctype? #:value (_cpointer _void)]
@defthing[_GgpFrameToken ctype? #:value (_cpointer _void)]
)]{
Be warned that these values are likely incorrect, and are apparently
@hyperlink["https://github.com/KhronosGroup/Vulkan-Docs/issues/1000"]{behind
an NDA}. Even if I knew what these were, I couldn't publish them here. If
you are the rare person who happened to sign this NDA and wish to use
Racket on Google Games, you may need to use a different binding. I
leave them here as @tt{void*} in the hopes you can use them and to clarify
that they are relevant to you.
}


@section{Spec Version Procedures}

Some core Vulkan signatures are implemented as C preprocessor macros
that are difficult to transform directly to Racket. Chief among them
are the expressions of specification versions that are relevant for
runtime and operational use. Their names are preserved as they appear
in C for consistency.

@defproc[(VK_MAKE_VERSION [major exact-nonnegative-integer?]
                          [minor exact-nonnegative-integer?]
                          [patch exact-nonnegative-integer?])
         exact-positive-integer?]{
This is a Racket variant of the @litchar{VK_MAKE_VERSION} macro from
@litchar{vulkan.h}.
}

@deftogether[(
@defthing[VK_API_VERSION_1_0 exact-positive-integer? #:value (VK_MAKE_VERSION 1 0 0)]
@defthing[VK_API_VERSION_1_1 exact-positive-integer? #:value (VK_MAKE_VERSION 1 1 0)]
)]{
Racket-specific Vulkan version values.
}

@deftogether[(
@defproc[(VK_VERSION_MAJOR [v exact-positive-integer?]) exact-positive-integer?]
@defproc[(VK_VERSION_MINOR [v exact-positive-integer?]) exact-positive-integer?]
@defproc[(VK_VERSION_PATCH [v exact-positive-integer?]) exact-positive-integer?]
)]{
Extracts version numbers from a value constructed by @racket[VK_MAKE_VERSION].
}


@section{Additional Procedures}

@defproc[(check-vkResult [v any/c] [who symbol?]) void?]{
Equivalent to @racket[(error who "failed: ~a" v)] if @racket[v] is not
a @tt{VkResult} success code. This procedure is implicitly applied
after all calls to foreign functions returning a VkResult.
}

@defproc[(format-vulkan-spec-version [spec-version exact-positive-integer?]) string?]{
Equivalent to:

@racketblock[
(format "~a.~a.~a"
  (VK_VERSION_MAJOR spec-version)
  (VK_VERSION_MINOR spec-version)
  (VK_VERSION_PATCH spec-version))
]
}
