main:
	ocamlbuild -pkgs graphics,ocamlgraph,unix planar.byte && ./planar.byte

clean:
	ocamlbuild -clean
