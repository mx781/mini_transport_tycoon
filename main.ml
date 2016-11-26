let () =
  try
    print_string
      "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
    print_endline
      "***********************************************************************";
    print_endline
      "               Welcome to Mini Transport Tycoon.                       ";
    print_endline
      "***********************************************************************";
    GameGraphics.draw_start ();
    print_endline "Please select:";
    print_endline "\t (1) to start a new game";
    print_endline "\t (2) to load a game from a file\n";
    print_string  "> ";
    let opt = read_line () in
    let file_name = if opt = "1" then "data/game.json" else if opt = "2" then (
    print_endline "\nPlease enter the name of the game file you want to load.\n";
    print_string  "> "; read_line () ) else failwith "You Lost The Game: you only had 2 options" in
    (* print_endline "Please select:";
    print_endline "\t (1) for a small display";
    print_endline "\t (2) for a medium display";
    print_endline "\t (3) for a large display.\n";
    print_string  "> "; *)
    let scale = "2"(* read_line () *) in
    print_string
      "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
    print_endline
      "***********************************************************************";
    print_endline "Insert Helpful Info Here:";
    print_endline "\tand here";
    print_endline "\t\tlots of";
    print_endline "\t\t\tvery helpful";
    print_endline "\t\t\t\tlines to explain";
    print_endline "\t\t\t\t\tthe game and what to do";
    print_endline
      "***********************************************************************";
    Engine.init_game file_name scale
  with
  | Graphics.Graphic_failure _ -> failwith "Don't Exit out of the Screen"
  | _ -> Engine.init_game "data/game.json" "2"