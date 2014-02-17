da-typing-shooter
=================

A shooter inspired by typespeed. To shoot an enemy, type the word on it.

To compile and try:

- Install first OCamlSDL2, OCamlSDL2_TTF, OCamlSDL2_Image, camomile.

 In order to use the Makefile, one must also add the following META contents into the sdl2 META file:

```
name = "sdl2"
description = "bindings to the SDL2 library"
license = "Zlib"
version = "@VERSION@"
directory = "+sdl2"

archive(byte) = "sdl2.cma"
+archive(native) = "sdl2.cmxa"
archive(byte,plugin) = "sdl2.cma"
archive(native,plugin) = "sdl2.cmxs"

package "ttf" (
   requires = "sdl2"
   archive(byte) = "sdl2_ttf.cma"
   archive(native) = "sdl2_ttf.cmxa"
)

package "image" (
   requires = "sdl2"
   archive(byte) = "sdl2_img.cma"
   archive(native) = "sdl2_img.cmxa"
)
```

- Install the GFS Didot font (or choose your preferred font using the --font-file switch).
- Install the french dictionary in /usr/share/dict/french (or choose your preferred dictionary file using the --dict-file switch).
- Then compile using make.
- Finally, call main.native to try.