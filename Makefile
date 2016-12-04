# Main needs to employ -use-ocamlfind to prevent duplicate inclusions of Unix
# that arise due to using myocamlbuild.ml.
main:
	ocamlbuild -use-ocamlfind -pkgs graphics,camlimages.png,camlimages.graphics,ocamlgraph,piqirun.ext,unix main.byte && ./main.byte

nice:
	ocamlbuild -use-ocamlfind -pkgs graphics,camlimages.png,camlimages.graphics,ocamlgraph,piqirun.ext,unix main.byte && ./main.byte

test:
	ocamlbuild -use-ocamlfind -pkgs oUnit,graphics,camlimages.png,camlimages.graphics,ocamlgraph,piqirun.ext,unix test.byte && ./test.byte

clean:
	ocamlbuild -clean
