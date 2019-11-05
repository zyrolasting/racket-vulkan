# Strangulation in progress
Some code is undergoing [strangulation][sp] as follows:

1. [x] `make-unsafe.rkt` uses `strangerfigs/writer.rkt` to write `unsafe.rkt` to the file system.
1. [ ] `make-unsafe.rkt` organizes code fragments from sequences in `stranglerfigs`, but does not
produce Racket code in sequences on its own.

Here's a breakdown of old and new for context.

## The old
The old code marked for death is a monolith that uses `analyze/spec.rkt`
to reorganize the entire Vulkan spec before processing in `generate/make-unsafe.rkt`.
It's hard to maintain because `make-unsafe.rkt` operates against all of the
assumptions set by `analyze/spec.rkt` at once. A new maintainer (or a forgetful author)
must re-check all assumptions.

## The new
The new code sits isolated from end users in `stranglerfigs`. It may grow independently,
but must follow these rules:

1. Generate Racket code fragments as [sequences][seq], not lists.
2. Each `X.rkt` module must place all tests in a `X.test.rkt` in the same directory.
3. Decouple the act of writing Racket modules from generating Racket code fragments that would appear within those modules.


[sp]: https://martinfowler.com/bliki/StranglerFigApplication.html
[seq]: https://docs.racket-lang.org/reference/sequences.html#%28tech._sequence%29

The program takes a Vulkan spec, `analyze`s it, and `generate`s Racket code.
