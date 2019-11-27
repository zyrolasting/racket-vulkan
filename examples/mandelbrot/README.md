This is a Racket port of @Erkaman's `vulkan_minimal_compute` example. To use
everything in this project, you will need the Vulkan SDK installed on your system.

There are two versions of the program. One uses generated bindings with added conveniences,
and another uses Vulkan's raw API directly. These are the `*-raw.rkt` files.

Once ready, just execute `racket mandelbrot.rkt` or `racket mandelbrot-raw.rkt`
to render the Mandelbrot set to a PNG. If the program starts too slowly,
run `raco make bindings*.rkt` first.

`bindings.rkt` was created with the following command:

```console
$ ravk generate --enable-auto-check-vkresult \
                --enable-symbolic-enums \
                ravk generate unsafe > bindings.rkt
```

`bindings-raw.rkt` was created using `ravk generate unsafe > bindings-raw.rkt`.
