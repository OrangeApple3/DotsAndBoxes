(* A player of the game.
 * ai=-1 denotes a human player. ai=n for n>=0 denotes the level of the AI. *)
type player = {ai: int; id: int; mutable score: int}

(** A coordinate on the game board, denoted by a row (letter)
 * and a column (number). Similar to how Chess boards are labeled.*)
type coord = {row: char; col: int}

(** An edge between two points on the board
 * Always the top or left coordinate of the edge *)
type edge = Vert of coord | Hor of coord

(** A completed box. A box has 4 points (corners)
 * Always the top left coordinate of the box *)
type box = Box of coord

(* A game board, consisting of dots, completed edges and completed boxes *)
type gameBoard = {
  points: coord list;
  edges: edge list;
  boxes: box list;
  c_edges: (edge*player) list;
  c_boxes: (box*player) list
}

(* The overall state of the game:
 * gameOver tracks whether or not the game has ended.
 * The player list is ordered with respect to sequence of turns. The id's are
 * ordered chronologically *)
type gameState = {
  players: player list;
  board: gameBoard;
  to_move: player;
  gameOver: bool
}

(* a player move, consisting of two coords representing adjacent vertices *)
type move = coord * coord

(* --- Game Logic --- *)

(* [validator gb x] determines if the player's move (x) is valid.
    A player's move is valid if:
    1. both vertices exist on the board gb
    2. the second vertex is adjacent to the first
    3. the edge defined by the move has not yet been claimed by a player *)
val validate : gameBoard -> move -> bool

(* [update_gb gb x] returns a new gameBoard formed from adding the
*  player's move (x) to the gameBoard gb. The player score IS NOT updated.
 * Note that this is the helper function for update_gs *)
val update_gb : gameBoard -> move -> player -> gameBoard

(* [update_gs gs x] returns a new gameState formed from adding the
 * player's move (x) to the gameState gs. The player score IS updated. *)
val update_gs : gameState -> move -> player -> gameState

(* [valid_moves gb x] returns a coord list of all adjacent coordinates to x not
 * already connected to x with an edge *)
val valid_coords : gameBoard -> coord -> coord list

(* Returns the list of (up to two) boxes that [edge] is adjacent to *)
val adjacent: gameBoard -> edge -> box list

val edges_of_box : box -> edge list

(* Returns a list of boxes completed by edge *)
val completed: gameBoard -> edge -> box list

(* Returns a list of all valid edges to move *)
val available_edges : gameBoard -> edge list

(* Returns the move associated to a given edge *)
val move_of_edge: edge -> move

(* Returns the edge associated to a given move *)
val edge_of_move: move -> edge

(* Returns a list of all valid moves to move *)
val available_moves : gameBoard -> move list

(* Returns uncompleted edges of a box *)
val edges_left: gameBoard -> box -> edge list

(* Returns a player with score zero, and the corresponding ai and id *)
val make_player: int -> int -> player

(* Returns the list of winning players. *)
val winners: gameState -> player list

(* Precondition: player list is nonempty
 * Creates a fresh gameState *)
val create_gs: player list -> gameBoard -> gameState