(* open Gamestate_piqi *)
open Gamestate_piqi_ext
(* open_inn Piqirun_custom *)

(* [parse_json_file file] parses a valid [file].json into a gamestate. *)
let parse_json_file file =
  let ch = open_in file in
  let rec read_line ch str =
    try
        match input_line ch with
        | s -> read_line ch (str ^ s)
    with
        | End_of_file -> str
  in 
  let str = read_line ch "" in
  close_in ch;
  parse_gamestate str `json


(* let _ =
  if Array.length Sys.argv <> 2
  then
    ( Printf.eprintf "Usage: %s ADDRESS_BOOK_FILE\n" Sys.argv.(0);
      exit (-1)
    );
  (* Read the existing address book. *)
  let ch = open_in_bin Sys.argv.(1) in
  let rec read_line ch str =
    try
        match input_line ch with
        | s -> read_line ch (str ^ s)
    with
        | End_of_file -> str
  in 
  let str = read_line ch "" in
  close_in ch;
  parse_gamestate str `json *)