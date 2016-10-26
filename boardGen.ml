open Game

let ($) l1 l2= List.fold_left (fun x -> fun y -> y::x) l2 l1;;

let rec gen_row (r:char) (coln:int) (c:coord) so_far : coord list =
  if coln = 0 then {row=r; col=c.col}::so_far
  else gen_row r (coln-1) c ({row=r; col=c.col+coln}::so_far)

let rows_to_char_list (rows:int) (c:coord) : char list =
  let rec help r lst =
    if r =(int_of_char c.row)-97 then lst
    else help (r-1) ((char_of_int (r+96)) :: lst) in
  help (rows + (int_of_char c.row)-96) []

let rect_board (rows:int) (cols:int) (c:coord)=
  let p= rows_to_char_list rows c in
  List.fold_left (fun x -> fun h -> x $ gen_row h cols c []) [] p

let in_board (c:coord list) (row:int) (col:int) (b:coord)  =
  List.for_all (fun x -> List.mem x c) (rect_board row col b)

(* Precondition: l has no duplicates
 * Postcondition: Returns the list of edges given a list of coordinates *)
let edge_list l =
  List.fold_left (fun x -> fun h -> Hor h::x) [] (List.filter (in_board l 0 1)
  l) $ List.fold_left (fun x -> fun h-> Vert h::x) [] (List.filter (in_board l
  1 0) l)

(* Precondition: l has no duplicates
 * Postcondition: Returns the list of boxes given a list of coordinates *)
let box_list l =
  List.fold_left (fun x -> fun h ->Box h::x) [] (List.filter
  (in_board l 1 1) l)

let standard_board rows cols =
  let p=rect_board rows cols {row='a';col=1} in
  {points=p; edges=edge_list p;boxes= box_list p;c_edges=[];c_boxes=[]}

let set_union (c1:coord list) (c2:coord list) : coord list =
  (List.filter (fun h -> not (List.mem h c2)) c1) $ c2

let valid_point (b:box list) (p:coord) =
  let a1=Box {row= char_of_int ((int_of_char p.row)-1); col =p.col} in
  let a2=Box {row= p.row;col= p.col-1} in
  let a3=Box {row= char_of_int ((int_of_char p.row)-1); col =p.col-1} in
  List.exists (fun x -> List.mem x b) [Box p; a1;a2;a3]

let random_board n m=
  let _= Random.init (int_of_float (Unix.time ())) in
  let rec help n1 (c1:coord list) =
    let a=List.nth (rect_board n n {row='a';col=1}) (Random.int (n*n)) in
    if n1=0 then c1 else help (n1-1) (set_union c1 (rect_board m m a)) in
  let p1=help (30/m) (rect_board m m {row='a';col=1} ) in
  let p= List.filter (fun h -> h.col <= n) (List.filter (fun h -> h.row <=
    char_of_int (96+n)) p1) in
  let q= List.filter (valid_point (box_list p)) p in
  {points=q ;edges= edge_list q;boxes=box_list q; c_edges =[]; c_boxes =[]}



