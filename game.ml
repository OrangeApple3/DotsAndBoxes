type player = {ai: int; id: int; mutable score: int}

type coord = {row: char; col: int}

type edge = Vert of coord | Hor of coord

type box = Box of coord

type gameBoard = {
  points: coord list;
  edges: edge list;
  boxes: box list;
  c_edges: (edge*player) list;
  c_boxes: (box*player) list
}

type gameState = {
  players: player list;
  board: gameBoard;
  to_move: player;
  gameOver: bool
}

type move = coord * coord

let list_contains l1 l2 = List.for_all (fun x -> List.mem x l2) l1

let point_of_box = function
  | Box x -> x

(* Returns a list of the upper left points of the boxes in l *)
let points_of_boxes l = List.map point_of_box l

(* Given an edge*player list returns the corresponding edge list *)
let c_list l = List.fold_left (fun x -> fun (h,_) -> h::x) [] l

let adjacent gb e =
  let ptlist = points_of_boxes gb.boxes in
  match e with
  | Vert x -> (let left = {row = x.row; col = x.col - 1} in
              if (List.mem x ptlist) && (List.mem left ptlist) then
                [Box x; Box left]
              else if List.mem x ptlist then
                [Box x]
              else if List.mem left ptlist then
                [Box left]
              else
                [])
  | Hor x -> (let up = {row =  char_of_int ((int_of_char x.row) - 1); col =
              x.col} in
              if (List.mem x ptlist) && (List.mem up ptlist) then
                [Box x; Box up]
              else if List.mem x ptlist then
                [Box x]
              else if List.mem up ptlist then
                [Box up]
              else
                [])

let edges_of_box b =
  let x = point_of_box b in
  [Vert x; Hor x; Vert {row = x.row; col = x.col + 1};
   Hor {row = char_of_int ((int_of_char x.row) + 1); col = x.col}]

let completed gb e =
  let (adj,edgelist)= (adjacent gb e, e::(c_list gb.c_edges)) in
  List.filter (fun hd -> list_contains (edges_of_box hd) edgelist) adj

let available_edges gb =
  let edgelist = c_list gb.c_edges in
  List.filter (fun x -> not (List.mem x edgelist)) gb.edges

let move_of_edge e =
  match e with
  | Vert x -> (x, {row = char_of_int ((int_of_char x.row) + 1); col = x.col})
  | Hor x -> (x, {row = x.row; col = x.col + 1})

let edge_of_move (c1,c2) =
  if c1.row = c2.row then Hor {row = c1.row; col = (min c1.col c2.col)}
  else Vert {row = (min c1.row c2.row); col = c1.col}

let available_moves gb = List.map move_of_edge (available_edges gb)

let edges_left gb b =
  let (e,c)= (edges_of_box b,c_list gb.c_edges) in
  List.filter (fun hd ->not (List.mem hd c)) e

let validate (gb : gameBoard) (m : move) : bool =
  List.mem m (available_moves gb)

let update_gb (gb : gameBoard) (m : move) (p:player) : gameBoard =
  let e=edge_of_move m in
  {gb with c_edges=(e,p)::gb.c_edges; c_boxes=List.map (fun x -> (x,p))
  (completed gb e) @ gb.c_boxes}

let update_gs (gs : gameState) (m : move) (p:player) : gameState =
  let n = p.id mod List.length gs.players in
  let p1 = List.nth gs.players n in
  let new_board=update_gb gs.board m p in
  match completed gs.board (edge_of_move m) with
  | [] -> {gs with board=new_board ; to_move=p1}
  | h::h1::t -> (p.score <- p.score+2); {gs with board=new_board;
                gameOver= List.length (available_moves new_board)=0}
  | h::t -> (p.score <- p.score+1); {gs with board=new_board;
            gameOver= List.length (available_moves new_board)=0}

let valid_coords (gb : gameBoard) (c : coord) : coord list =
  let e= List.filter (fun x -> List.mem x (available_edges gb))
  [Hor c;Vert c;Hor {row= c.row;col= c.col-1}; Vert {row = char_of_int
  ((int_of_char c.row)-1); col =c.col}] in
  List.map (fun x ->
            match x with
            | Hor i -> if i=c then {row= c.row;col= c.col+1}
                       else {row= c.row;col= c.col-1}
            | Vert i -> if i=c then
                          {row= char_of_int ((int_of_char c.row)+1); col =c.col}
                        else
                          {row= char_of_int ((int_of_char c.row)-1); col =c.col}
           ) e

(* Returns a player with score zero, and the corresponding ai and id *)
let make_player (ai:int) (id:int) : player =
  {ai;id;score=0}

(* Returns the id of the winning player *)
let winners (gs:gameState) : player list =
  let big=List.fold_left (fun x -> fun y -> if y.score > x then y.score else x)
  0 gs.players in List.filter (fun x -> x.score=big) gs.players

let create_gs pl b = {
  players = pl;
  board = b;
  to_move = List.hd pl;
  gameOver = false
  }