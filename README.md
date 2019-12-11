[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/vulkan/index.html)
[![](https://tokei.rs/b1/github/zyrolasting/racket-vulkan)](https://github.com/zyrolasting/racket-vulkan)

<p align="center">
  <img style="width: 100%" alt="Racket + Vulkan® = awesome" src="./private/assets/hero.png">
</p>

Vulkan® API integration and utilities for Racket.

_Vulkan and the Vulkan logo are registered trademarks of the Khronos Group Inc._

## What you get
- [x] Faithful replication of the raw Vulkan API
- [x] Opt-in safety and conveniences
- [x] Powerful code generators that target any `vk.xml`
- [x] Vulkan 1.1 support, and the means to adapt to future specs
- [x] All platform bindings exposed

## Quick start

```console
$ raco pkg install vulkan
$ ravk generate unsafe > unsafe.rkt
```

This generates Racket bindings using the local `vk.xml` mirror that
comes with the package. From here, you can `(require "unsafe.rkt")`
and use Vulkan from the dependent module [according to the
manual][unsafe].

[unsafe]: https://docs.racket-lang.org/vulkan/Unsafe_Bindings.html
