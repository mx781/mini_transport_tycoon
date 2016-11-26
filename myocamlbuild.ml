(* OCamlbuild plugin that grants module visibility between different dirs.
 * Here, the main dir can access Piqirun stuff in piqi/, while piqi/ modules
 * can access GameElements for types. *)

open Ocamlbuild_plugin
 
let () =
  dispatch begin function
  | After_rules -> 
     Pathname.define_context "" ["piqi"];
     Pathname.define_context "piqi" [""]
  | _ -> ()
  end
