[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/vulkan/index.html)

<p align="center">
  <img style="width: 100%" alt="Racket + Vulkan® = awesome" src="./private/assets/hero.png">
</p>

Vulkan® API integration and utilities for Racket.

_Vulkan and the Vulkan logo are registered trademarks of the Khronos Group Inc._

## Supporting the project
I actively maintain my open source projects, and
wish to do so full-time. If you want to support unique
software and get guarenteed response times for support, consider
[purchasing a subscription](https://sagegerard.com/subscribe.html).


## 30-second Code Tour
This section is for maintainers, for the curious, and for myself when
I inevitably forget what I did.

This project's chief job is to generate Racket code that uses Vulkan.
`spec.rkt` manages Vulkan's XML specification. The local mirror is
stored in `private/assets/vk.xml`.  The X-expression form of that
document acts as the input to all code generators.

`private/generate` contains the code generators. Each module therein
provides an `in-fragment` procedure.  `in-fragment` lazily produces
some sequence of Racket code from a Vulkan specification. Each module
will tell you about the code it produces and the assumptions it makes
in doing so. You can pass fake specifications to experiment.

`private/writer.rkt` implements the `raco rvk-gen` command. Pass
a path to one of the `private/generate` modules to see the code
that module produces.
