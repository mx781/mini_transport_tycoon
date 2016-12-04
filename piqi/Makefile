# for testing purposes only
main:
	ocamlbuild -pkgs piqirun.ext,ocamlgraph dataProcessing.byte

piqi:
	piqic-ocaml --multi-format player.piqi; piqic-ocaml --multi-format graph.piqi; piqic-ocaml --multi-format gameelements.piqi; piqic-ocaml --multi-format gamestate.piqi;
	
clean:
	ocamlbuild -clean
