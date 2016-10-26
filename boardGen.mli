open Game

(* [standard_board m n] generates an m x n board *)
val standard_board: int -> int -> gameBoard

(* [random_board n m] generates a random board, with density level m>0,
 * that is contained inside an n x n board. *)
val random_board : int -> int -> gameBoard
