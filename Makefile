main:
	ocamlbuild -pkgs graphics,ocamlgraph,unix main.byte && ./main.byte


planar:
	ocamlbuild -pkgs graphics,ocamlgraph,unix planar.byte && ./planar.byte

clean:
	ocamlbuild -clean
