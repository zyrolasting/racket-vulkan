#lang scribble/manual

@title{Contributing}

The official repository @hyperlink["https://github.com/zyrolasting/racket-vulkan"]{is on GitHub}.
The source code tracks the @hyperlink["https://www.khronos.org/registry/vulkan/"]{Vulkan Registry}.

@section{What's Needed Right Now}
This collection depends on contributions of code or funding to keep up
with the rapid development of Vulkan. The GitHub README shows funding
oppurtunities, while this document focuses on technical contributions.

@bold{Right now the project has a strong need for example Vulkan
applications.} You can make an impact by porting C programs to Racket.
Each such application acts as a functional test for continuous
integration purposes, which covers far more than unit tests
ever could.

@section{Maintenance Guide}
This section is only meant to aid high-level understanding
of the project and its technical needs. It is not a reference
for implementation details.

@subsection{System Design}
This project's job is to generate Racket code that uses Vulkan
according to an end user's assumptions.

The system must remain flexible in the face of any change to a
machine-readable Vulkan specification. It does this by giving an
@italic{end user} the means to keep their Racket code up to date with
Vulkan, even if the source code of this project is @italic{out of
date}.

This is possible because the specification and code generators can
change independently, and are accessible through a public API.

@tt{vk.xml} in particular acts as input to all code generators in the
project. Each generator lazily produces a sequence of code from a
specification according to its own assumptions. You have to know how
to apply generators together to do something meaningful. I did it this
way because even backwards-compatible changes to a Vulkan
specification can break a model of it, so having a collection of
specialized generators makes it easier to manage assumptions.

@subsection{Maintenance Tasks}
This project maintains its own mirror of @tt{vk.xml} so it can
function offline, and the built-in code generators track its
contents. To update the project, maintainers should periodically write
a new version of the XML mirror, regenerate a Vulkan integration, and
run all relevant tests.

I normally update like so, assuming the package is installed on my
system using @litchar{raco pkg install --link}. I do not use the
actual paths to the relevant files in the event they move around the
project.

@verbatim[#:indent 4]|{
# Download the latest vk.xml and replace the local mirror
ravk --latest show spec > /path/to/vk.xml

# Regenerate the unsafe FFI bindings.
# If this errors out, a code generator hit a runtime error
# and is likely incompatible with the XML update.
ravk replicate > /path/to/unsafe.rkt

# Recompile the project.
# If this errors out, the unsafe bindings contain
# incorrect Racket code. This points to a logic
# error in a code generator.
raco setup vulkan

# Run unit tests
raco test --jobs 8 --drdr -p vulkan

# If Vulkan is actually installed on the system,
# use Racket or GRacket to use example
# applications as functional tests.
racket /path/to/examples/app1.rkt
racket /path/to/examples/app2.rkt
...
}|

Going through this script creates confidence in the package
and a Vulkan specification upgrade.

Should a new @tt{vk.xml} undergo a backwards-incompatible change, the
code generators should be archived such that they apply to prior
versions. A new set of code generators should henceforth process
the local mirror. There is not yet a way to pick only relevant
code generators given a spec version, but the implementation
will come when it's needed.
