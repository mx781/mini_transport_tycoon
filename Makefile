main:
	ocamlbuild -pkgs graphics,camlimages.all,ocamlgraph,unix main.byte && ./main.byte


planar:
	ocamlbuild -pkgs graphics,ocamlgraph,unix planar.byte && ./planar.byte

run:
	ocamlbuild -pkgs graphics,ocamlgraph game.byte && ./game.byte

clean:
	ocamlbuild -clean
