open Game

(* Removes an element from a list, if it is in it *)
let rec remove x l =
  match l with
  | [] -> []
  | hd::tl -> (if x = hd then
                remove x tl
              else
                hd::(remove x tl))

(* Removes l1 from l2 *)
let rec remove_list l1 l2 =
  match l1 with
  | [] -> l2
  | hd::tl -> remove_list tl (remove hd l2)

(* Precondition: e completes box b in gb
 * Checks to see if another box can be completed after already completing one *)
let rec chain_check gb e b =
  let adj = adjacent gb e in
  let adj2 = remove b adj in (* has length at most 1 *)
  match adj2 with
  | [] -> 1
  | bhd::_ -> (let eleft = remove e (edges_left gb bhd) in
              match eleft with
              | [] -> 2
              | ehd::etl -> (if List.length etl = 0 then
                              1 + chain_check gb ehd bhd
                            else
                              1))

let check_points gb e  =
  let boxlist = completed gb e in
  if List.length boxlist = 1 then
    let box = List.hd boxlist in
    chain_check gb e box
  else
    List.length boxlist

(* Returns a edge*int list that gives the points associated with each edge
 * in el*)
let edge_list_points gb el =
  let points_tup e =
    (e, check_points gb e) in
  List.map points_tup el

(* Precondition: int components of each element of l are nonnegative
 * Returns maximum number of points from a edge*int list *)
let rec max_points l =
  match l with
  | [] -> 0
  | hd::tl -> max (snd hd) (max_points tl)

(* Precondition: l is a edge*int list where each element has the same int component
 * If x is greater than the int component of the elements of l, returns [x]
 * If x is less than the int component of the elements of l, returns l
 * If x is equal to the int component of the elements of l, returns x::l *)
let greater l x =
  let pts1 = snd (List.hd l) in
  let pts2 = snd x in
  if pts1 > pts2 then
    l
  else if pts1 < pts2 then
    [x]
  else
    x::l

(* Returns a  edge*int list of the best moves for the current player, without
 * considering other players' moves *)
let best_move_once gb =
  let points = edge_list_points gb (available_edges gb) in
  match points with
  | [] -> []
  | hd::tl -> List.fold_left greater [(fst hd, min_int)] points

(* For edges that give score 0, corrects the score to account for the number of
 * points the edge allows the next player to score *)
let best_move_helper gb plr =
  let bestmoves = best_move_once gb in
  let rec helper (l:(edge*int) list) =
    match l with
    | [] -> []
    | hd::tl -> (if snd hd = 0 then
                  let gb2 = update_gb gb (move_of_edge (fst hd)) plr in
                  let pts = max_points (best_move_once gb2) in
                  (fst hd, 0 - pts)::(helper tl)
                else
                  hd::(helper tl)) in
  helper bestmoves

(* Precondition: l is nonempty
 * Returns a random edge from l *)
let random_element l =
  let n = List.length l in
  List.nth l (Random.int n)

(* Precondition: n >= 0
 * This implementation only searches 2 moves into the future. This is
 * (partially) because in a game with multiple players, the minimax algorithm
 * cannot be applied (directly). Indeed, in this case the player and the next
 * player are not playing a zero sum game.
 * In particular, any difficulty setting >= 2 will be the same. Thus there are
 * exactly 3 difficulty settings for AI.
 * At the lowest difficulty setting, AI plays randomly
 * At the middle difficulty setting, AI plays the move for which he gains the
 * most points without considering how other players will react. In particular,
 * if all moves give the AI 0 points, it may play a move that gives the next
 * player points (even if this move is not optimal)
 * At the highest difficulty setting, AI plays the move for which he gains the
 * most points, or otherwise plays the move that allows the next player to score
 * the fewest number of points. *)
let best_move gs n =
  if n = 0 then
    let bestedge = random_element (available_edges gs.board) in
    move_of_edge bestedge
  else if n = 1 then
    let bestmoves = best_move_once gs.board in
    let bestedge = fst (random_element bestmoves) in
    move_of_edge bestedge
  else
    let allmoves = best_move_helper gs.board gs.to_move in
    let bestmoves = match allmoves with
                    | [] -> []
                    | hd::tl -> List.fold_left greater [(fst hd, min_int)] allmoves in
    let bestedge = fst (random_element bestmoves) in
    move_of_edge bestedge

(* Like best_move, but returns a list of all the best moves
 * Useful for testing as there is no randomness
 * Note: Though this returns a list of moves, it is easier to assert edges are
 * equal, using edge_of_move. This is because every move has two representations
 * (switching the components) but edges have a single unique representation. *)
let best_move_test gs n =
  if n = 0 then
    List.map move_of_edge (available_edges gs.board)
  else
  let helper x =
    move_of_edge (fst x) in
  if n = 1 then
    let bestmoves = best_move_once gs.board in
    List.map helper bestmoves
  else
    let allmoves = best_move_helper gs.board gs.to_move in
    let bestmoves = match allmoves with
                    | [] -> []
                    | hd::tl -> List.fold_left greater [(fst hd, min_int)] allmoves in
    List.map helper bestmoves
