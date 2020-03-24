# Changelog

This file summarizes changes to the project over time.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/).

Each version is in the format `major.minor`. `minor` and `major`
increments are subject to the author's interpretation of the words,
but generally mean that `major` changes have "bigger" impact than
`minor` changes. That said, _breaking changes have no relationship to
version numbers._ By policy, any breaking change is simply avoided
unless there's a good reason to introduce one. It's [too difficult to
draw a reliable numerical relationship][jash], since some breaking
changes are meant to prepare intended functionality. You should not
upgrade until you have reason or desire to do so. If you are starting
out with this project, then you should use the latest version.

## [Unreleased]
Nothing yet.

## [1.2] - 2020-03-23
* Synchronized implementation to v1.2 specification
* Patch bug where generated union declarations have `void` members.

## [1.1] - 2019-11-12
* Add missing `compatibility-lib` declaration

## [1.0] - 2019-11-15
* Add `ravk` CLI
* Refactor to use distributed code generators
* Reorganize modules
* Publish `vulkan/spec`, and `vulkan/unsafe`
* Add example applications

## [0.0] - 2019-08-24
* Start Racket package
* Add simple model for working with `vk.xml`
* Work on monolithic code generator
* Add tiny examples

[Unreleased]: https://github.com/zyrolasting/racket-vulkan/compare/v1.2...HEAD
[1.2]: https://github.com/zyrolasting/racket-vulkan/compare/v1.1...v1.2
[1.1]: https://github.com/zyrolasting/racket-vulkan/compare/v1.0...v1.1
[1.0]: https://github.com/zyrolasting/racket-vulkan/compare/v0.0...v1.0
[0.0]: https://github.com/zyrolasting/racket-vulkan/releases/tag/v0.0

[jash]: https://gist.github.com/jashkenas/cbd2b088e20279ae2c8e
