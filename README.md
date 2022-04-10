# drawing-demo

_Very basic drawing app to demonstrate the use of
[Bogue](https://github.com/sanette/bogue)._

Want to train your handwriting, or draw with constraints? You're on
the right spot. This app features:

* very fast reaction to mouse/touchscreen/stylus
* only thin strokes
* "Clear" button with confirmation popup
* a limited number of colors
* unlimited undo
* unlimited creativity!

![screenshot](drawing-demo.png)

## install

If you run linux, you can try executing the
[compiled binary](drawing_x86_64).

Otherwise, you need to install bogue (>= 20220408) and compile the
drawing-demo with

```
dune exec ./drawing.exe
```

Of course, in any case you will need to install SDL2, too. Either via
`apt`: install the packages `libsdl2-image-2.0-0` and
`libsdl2-ttf-2.0-0`, or by compiling SDL2 yourself if you want a newer
version. (SDL 2.0.10 has bugs when drawing lines.)
