open Game


(* What a printed board looks like (note it can be non-rectangular 
 *
 * a       o   o   o   o   o   o
 *         |           |
 * b   o---o---o---o---o   o   o
 *     |   | 1 | 2     |   
 * c   o   o---o---o---o   
 *         |        
 * d   o---o---o   o   o   o  
 *             |   |   |
 * e   o   o   o   o   o---o 
 *                 |
 * f       o   o   o   o
 * 
 *     1   2   3   4   5   6   7
 *
 *  Lines are colored according to their owner,
 *    e.g. Player 1 might be blue, Player 2 red
 *  Completed boxes are lableled with a colored number indicating which
 *    player completed them
 *)


(* [build_horiz board r] returns a ready-for-printing string representing
 * row r of a board.
 * in the example above, [build_horiz board 'b'] would produce the string
 * "b   o---o---o---o---o   o   o" with correct colors
 *)
val build_horiz: gameBoard -> char -> string

(* [build_vert board r] returns a ready-for-printing string representing
 * the space below row r.
 * In the above example, [build_vert board 'b'] would produce the string
 * "    |   | 1 | 2     |" with correct colors
 *)
val build_vert : gameBoard -> char  -> string


(* [print_board b] Prints gameBoard [b] to standard out, nicely formatted
 * and human readable *)
val print_board: gameBoard -> unit

(* [print_score gs] prints the scores of all players in the game *)
val print_score: player list -> unit

(* [print_state gs] is print_board gs.board and print_score gs *)
val print_state: gameState -> unit



(* for testing *)
val get_row_points: char -> coord list -> coord list
