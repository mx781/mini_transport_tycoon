main:
	ocamlbuild -use-ocamlfind -package piqirun.ext dataProcessing.byte

piqi:
	piqic-ocaml --multi-format player.piqi; piqic-ocaml --multi-format graph.piqi; piqic-ocaml --multi-format gameelements.piqi; piqic-ocaml --multi-format gamestate.piqi;

manual-old:
	ocamlfind ocamlc -linkpkg -package piqirun.ext player_piqi.ml player_piqi_ext.ml gameelements_piqi.ml gameelements_piqi_ext.ml graph_piqi.ml graph_piqi_ext.ml gamestate_piqi.ml gamestate_piqi_ext.ml dataProcessing.ml -o dataProcessing.byte;