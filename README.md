Vulkan API integration and utilities for Racket.

## How to test

1. Clone this repository and use `raco pkg install --link`.
1. Install the Vulkan SDK or runtime. Make sure the associated C
   library appears inside one of the directories from
   [get-lib-search-dirs][glsd]
1. Run the unit tests using `raco test -x -p vulkan`
1. Run `racket examples/minimal.rkt` in the repository to make sure you can create and destroy an instance.

If you need to modify `unsafe.rkt`, be sure to run `raco make` on it to capture any changes.
It takes a while to compile.

## TODO

- [x] Generate Racket FFI bindings against the Vulkan 1.1 API specification.
- [ ] Include RAII-equivalent resource management to reduce verbosity
- [x] Write a test application that generates an image and writes it to the file system
- [ ] Write a test application that uses a GUI and swap chain to present graphics.
- [x] Write example applications to aid education

[glsd]: https://docs.racket-lang.org/raco/dirs.html#%28def._%28%28lib._setup%2Fdirs..rkt%29._get-lib-search-dirs%29%29
