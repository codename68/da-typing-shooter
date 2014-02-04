all:
	ocamlbuild -pkgs sdl2,sdl2.ttf,sdl2.image,camomile main.native

debug:
	ocamlbuild -lflags -custom -pkgs sdl2,sdl2.ttf,sdl2.image,camomile main.d.byte

.PHONY: all debug
