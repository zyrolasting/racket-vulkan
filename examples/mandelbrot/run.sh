#! /bin/bash

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cd $HERE
echo "Compiling mandelbrot.rkt"
raco make mandelbrot.rkt

echo "Running mandelbrot.rkt"
racket mandelbrot.rkt

echo "Converting output RGBA dump"
convert -size 3200x2400 -depth 8 mandelbrot.rgba mandelbrot.png

echo "Cleaning up"
rm mandelbrot.rgba

echo "Done. Output: $HERE/mandelbrot.png"
