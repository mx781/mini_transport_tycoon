let () =
  try
    print_string
      "\n\nWelcome to Mini Transport Tycoon.\n";
    GameGraphics.draw_start ();
    print_endline "Please enter the name of the game file you want to load.\n";
    print_string  "> ";
    let file_name = read_line () in
    print_endline "Please select:";
    print_endline "\t (1) for a small display";
    print_endline "\t (2) for a medium display";
    print_endline "\t (3) for a large display.\n";
    print_string  "> ";
    let scale = read_line () in
    Engine.init_game file_name scale
  with
  | Graphics.Graphic_failure _ -> failwith "Don't Exit out of the Start Screen"
  | _ -> Engine.init_game "default.json" "2"