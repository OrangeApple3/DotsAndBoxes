open Game

(* prints [text] in a color determined by [number], which should correspond
 * to a player id number
 *  1 - Red
 *  2 - Green
 *  3 - Yellow
 *  4 - Blue
 *  Other numbers might do weird things 
 *
 *  (7 == white and is used for hints)
 *
 *  From the example in Real World OCaml, chapter 6 *)
let color_by_id number text = 
  Printf.sprintf "\027[38;5;%dm%s\027[0m" number text

(* a comparisson function for ordering points in a row *)
let pointcompare_horiz p1 p2 = p1.col - p2.col

(* a comparisson function for ordering edges in a row *)
let edgecompare (e1: edge*player) (e2:edge*player)  =
  let x = match (fst e1) with | Hor e -> e.col | Vert e -> e.col in
  let y = match (fst e2) with | Hor e -> e.col | Vert e -> e.col in
  x - y

(* returns a sorted list of the points in a specified row *)
let get_row_points row points  =
  let rec get row points = 
    match points with
      | [] -> []
      | x::xs ->
        if x.row = row then x::(get row xs)
        else get row xs
  in
  let lst = get row points in
  List.sort (fun x -> fun y -> x.col-y.col) lst

(* returns a sorted list of completed horizontal edges in a specified row *)
let get_row_edges row c_edges  = 
  let rec get row c_edges = 
    match c_edges with
      | [] -> []
      | x::xs ->
          ( match fst x with
            | Hor c when c.row = row -> x::(get row xs)
            | _ -> get row xs
          )
  in
  let lst = get row c_edges in
  List.sort edgecompare lst

(* returns a sorted list of completed vertical whose top is in row *)
let get_vert_edges row c_edges = 
  let rec get row c_edges = 
    match c_edges with
      | [] -> []
      | x::xs ->
          ( match fst x with
            | Vert c when c.row = row -> x::(get row xs)
            | _ -> get row xs
          )
  in
  let lst = get row c_edges in
  List.sort edgecompare lst

(* returns a string that is a representation of the edge to the right of p in 
 * the same row, or a blank space if there is no edge *)
(* one iteration might look like "o---"*)
let gap_horiz p edges = 
    try 
      let player = List.assoc (Hor p) edges in
      color_by_id player.id "---"
    with Not_found -> "   "

(* returns a string with the proper amount of spaces to represent larger gaps
 * in the board *)
let long_gap p1 p2 = 
  let dist = p2.col - p1.col in

  let rec build n so_far =
    if n <= 1 then so_far
    else build (n-1) so_far^"    "
  in

  build dist "   "

(* one iteration might look like "| 1 " *)
let gap_vert p edges boxes = 
  let edgestring = 
    try
      let p_edge = List.assoc (Vert p) edges in
      color_by_id p_edge.id "|"
    with Not_found -> " "
  in
  let boxstring = 
    try 
      let p_box = List.assoc (Box p) boxes in
      color_by_id p_box.id (Printf.sprintf " %i " p_box.id)
    with Not_found -> "   "
  in
  edgestring^boxstring

(* adds spacing representing large gaps in the vertical edge space. 
 * Very similar to long_gap, but spacing is slightly different *)
let long_gap_vert p1 p2 = 
  let dist = p2.col - p1.col in
  let rec build n so_far = 
    if n <= 1 then so_far
    else build (n-1) so_far^"    "
  in
  build dist ""



(* builds a string representing one row of the board *)
let build_horiz board row : string = 
  let points = get_row_points row board.points in
  let edges  = get_row_edges row board.c_edges in

  (* precondition: points is sorted *)
  let rec build points edges so_far (prev: coord option) = 
    match points,prev with
      | ([],_) -> so_far
      | (x::xs, None) -> (* this is the first point in the row *)
          if x.col = 1 then 
            build xs edges (so_far^"o") (Some x)
          else
            build xs edges (so_far^" "^(long_gap {row=row; col=1} x)^"o") (Some x)
      | (x::xs, Some p) ->
          let res = 
            if p.col = (x.col - 1) then gap_horiz p edges
            else long_gap p x
          in
          build xs edges (so_far^res^"o") (Some x)
  in
  build points edges (Printf.sprintf "%c   " row) None



(* builds the string representing the space below a given row, consisting
 * of vertical edges and box ownership markers *)
let build_vert board row : string = 
  let points = get_row_points row board.points in
  let edges = get_vert_edges row board.c_edges in

  (* precondition: points is sorted *)
  let rec build points edges boxes so_far (prev: coord option) =
    match (points, prev) with
      | ([],_) -> so_far
      | (x::xs, None) -> (* this is the first point in the row *)
          let res = gap_vert x edges boxes in
          if x.col = 1 then
            build xs edges boxes (so_far^res) (Some x)
          else
            let indent = long_gap {row=row; col=1} x in
            build xs edges boxes (so_far^" "^indent^res) (Some x)
      | (x::xs, Some p) ->
          let res = gap_vert x edges boxes in
          if x.col - p.col = 1 then
            build xs edges boxes (so_far^res) (Some x)
          else
            let indent = long_gap_vert p x in
            build xs edges boxes (so_far^indent^res) (Some x)
  in
  build points edges board.c_boxes "    " None


(* builds the string of column numbers along the bottom of the board *)
let build_colnums num_cols = 
  let rec make n nmax = 
    if n > nmax then ""
    else 
      let res = 
        if (n/10=0) then Printf.sprintf "%i   " n
        else Printf.sprintf "%i  " n
      in
      res^(make (n+1) nmax)
  in
  make 1 num_cols


(* builds a string represnting the board as a whole *)
let build_board board = 
  let ilst = List.map (fun p -> int_of_char p.row) board.points in
  let imax = List.fold_left max min_int ilst in
  let imin = List.fold_left min max_int ilst in

  let rec make board n nmax so_far =
    if n > nmax then so_far
    else
      let h = build_horiz board (char_of_int n) in
      let v = build_vert  board (char_of_int n) in
      make board (n+1) nmax (so_far^(Printf.sprintf "%s\n%s\n" h v))
  in
  let s = make board imin imax "" in
  let cols = List.map (fun p -> p.col) board.points in
  let num_cols = List.fold_left max min_int cols in
  Printf.sprintf "%s    %s\n" s (build_colnums num_cols)


(* prints the board, nicely formatted and beautiful *)
let print_board board = Printf.printf "%s" (build_board board)

(* prints scores of all players *)
let print_score players = 
  let playercompare p1 p2 = p1.id - p2.id in
  let sorted = List.sort playercompare players in

  let rec build players so_far = 
    match players with
      | [] -> so_far
      | p::ps ->
          let ai = if p.ai >= 0 then " (ai)" else "" in
          let s = color_by_id p.id (Printf.sprintf "Player %i%s: " p.id ai) in
          let score = Printf.sprintf "%i" p.score in
          build ps (so_far^"    "^s^score)
  in
  let line = build sorted "Scores: " in
  Printf.printf "%s\n" line


let print_state gs = 
  Printf.printf "\n\n";
  print_board gs.board;
  print_score gs.players
