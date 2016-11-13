let () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to Mini Transport Tycoon.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  let file_name = read_line () in
  Engine.main file_name