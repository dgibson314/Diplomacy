all: board init gui utils engine start toplevel

board: board.ml
	ocamlbuild -pkg str board.native

init: init.ml
	ocamlbuild -pkg str init.native

gui: gui.ml
	ocamlbuild -pkgs lablgtk2,str,async,unix gui.native

start: start.ml
	ocamlbuild -r -pkgs lablgtk2,str start.native

utils: game_utils.ml
	ocamlbuild -pkg str game_utils.native

engine: engine.ml
	ocamlbuild -pkgs lablgtk2,str,async,unix engine.native

toplevel: toplevel.ml
	ocamlbuild -pkgs lablgtk2,str,async,unix toplevel.native

clean:
	rm -rf _build *.native
