open Game


type command = Help | Quit | Hint | Move of move


(** [help] prints a standardized help screen *)
val help : unit -> unit

(** [quit] terminates the program *)
val quit : unit -> unit

(** [hint gs] prints the game board with a new edge added, representing
 * what the ai thinks is the best move *)
val hint : gameState -> unit

(** [prompt_and_parse gs] prompts the player whose turn it is, parses the
 * input, and returns a command accordingly *)
val prompt_and_parse : gameState -> command

(** The entry point of the program. Prompts the user to start a new game
 * or load a saved one, then calls game_loop to begin the game loop. *)
val main_menu  : unit -> gameState

(** The main loop of the game. One iteration of the loop consists of displaying
 * the board for the user, making user and AI moves, and returning a new 
 * GameState *)
val game_loop  : gameState -> gameState


