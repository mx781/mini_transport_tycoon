open Graphics
open GameElements
open Player
open InputProcessing

(* Truncates floats to two digits. *)
val two_dec : float -> float

(* Matches mouse clicks to processes.*)
val click_buttons : game_state -> int -> process

(* Used to determine what happens when a winner exists. *)
val draw_winner : int -> game_state -> unit

(* Draws the whole game.*)
val draw_game_state : game_state -> unit

(* Draws the title screen. *)
val draw_start : unit -> unit

(* Determines what to do during the title screen*)
val title_click : unit -> int

(* Used for determining difficulty settings*)
val settings : unit -> ai_game_difficulty