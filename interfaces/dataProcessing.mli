(* Local file processing *)
(* load local file [string] and convert to our game_state type*)
val load_file: string -> game_state

(* convert game_state to json file and save it as [file]*)
val save_file: game_state -> string -> unit

(* OSM-related, tentative *)
val get_city_uri: string -> string
val load_uri: string -> game_state