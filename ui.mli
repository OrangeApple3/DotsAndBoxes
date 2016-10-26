open Game

(* the types of commands a player can enter *)
type command =
  | Help | ScoreChk | Exit | Move of move

(* [help] prints a standardized help screen *)
val help : gameState -> gameState

(* [score_check n] prints the given player (player n's) score *)
val score_check : gameState -> gameState

(* [quit] terminates the program *)
val quit : unit -> unit

(* [move_parser] takes user-entered coordinates, checks that they are valid,
 * and returns a new gameState using game.update_gs) *)
val move_parser : gameState -> string -> gameState

(* [prompt gb] takes in a gameBoard and returns a gameBoard reflecting the
    results of the user's move (if there is one). Keep prompting until the
    user exits the program or makes a valid move *)
val prompt : gameState -> gameState
