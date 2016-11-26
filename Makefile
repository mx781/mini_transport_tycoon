# Main needs to employ -use-ocamlfind to prevent duplicate inclusions of Unix 
# that arise due to using myocamlbuild.ml.
main:
	ocamlbuild -use-ocamlfind -pkgs graphics,camlimages.png,camlimages.graphics,ocamlgraph,piqirun.ext,unix main.byte && ./main.byte

planar:
	ocamlbuild -pkgs graphics,ocamlgraph,unix planar.byte && ./planar.byte

clean:
	ocamlbuild -clean
