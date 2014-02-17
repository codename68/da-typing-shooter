all:
	ocamlbuild -pkgs sdl2,sdl2_ttf,sdl2_img,camomile main.native

debug:
	ocamlbuild -lflags -custom -pkgs sdl2,sdl2_ttf,sdl2_img,camomile main.d.byte

.PHONY: all debug
