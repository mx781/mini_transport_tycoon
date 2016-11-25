main:
	ocamlbuild -pkgs graphics,camlimages.png,camlimages.graphics,ocamlgraph,unix main.byte && ./main.byte


planar:
	ocamlbuild -pkgs graphics,ocamlgraph,unix planar.byte && ./planar.byte

clean:
	ocamlbuild -clean
