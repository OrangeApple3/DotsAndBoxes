open Game

(** Returns the number of points an edge yields at the given game state *)
val check_points: gameBoard -> edge -> int

(** Returns a guess for the best move. The second argument determines the
 * difficulty setting of the AI, with 0 being the easiest *)
val best_move: gameState -> int -> move

(** See best_move. Returns a list of all the guesses for the best move.
 * For use in testing *)
val best_move_test: gameState -> int -> move list
