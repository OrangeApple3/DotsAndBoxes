open Game
open Printer
open BoardGen

(* generate a simple set of coords for testing *)
let rec gen_row r n so_far : coord list = 
    if n = 0 then so_far
    else gen_row r (n-1) ({row=r; col=n}::so_far)

let rows_to_char_list rows : char list = 
  let rec alpha_of_int i : char = 
    let of_int i =  (char_of_int (i - 1 + int_of_char 'a')) in
    (of_int (i mod 26))
  in
  
  let next = ref 0 in
  let newchar () = 
    next := 1 + !next;
    alpha_of_int !next
  in

  let rec make n = 
    if n = 0 then []
    else newchar()::(make (n-1))
  in
  List.rev (make rows)


let gen_coords rows cols : coord list = 
  let rows_c = rows_to_char_list rows in
  let rec gen rowlst cols so_far = 
    match rowlst with
      | [] -> so_far
      | h::t ->
          let r = gen_row h cols [] in
          gen t cols (so_far @ r)
  in
  gen rows_c cols []


TEST_UNIT "simple coord generator works" = 
  let simple = gen_coords 3 3 in
  assert (simple = [{row='a'; col=1}; {row='a'; col=2}; {row='a'; col=3};
                    {row='b'; col=1}; {row='b'; col=2}; {row='b'; col=3};
                    {row='c'; col=1}; {row='c'; col=2}; {row='c'; col=3};]);
  Printf.printf "coord gen tests complete\n"


TEST_UNIT "get_row_points works" = 
  let simple = gen_coords 3 3 in
  assert (Printer.get_row_points 'a' simple = 
    [{row='a'; col=1}; {row='a'; col=2}; {row='a'; col=3}]);
  assert (Printer.get_row_points 'b' simple = 
    [{row='b'; col=1}; {row='b'; col=2}; {row='b'; col=3}]);
  assert (Printer.get_row_points 'c' simple = 
    [{row='c'; col=1}; {row='c'; col=2}; {row='c'; col=3}]);
  Printf.printf "get_row_points tests complete\n"


TEST_UNIT "build_horiz works" = 
  let player1 = {ai=(-1); id=1; score=0} in
  let player2 = {ai=(-1); id=2; score=0} in
  let coords = gen_coords 3 3 in

  let edge1 = Hor ({row='a'; col=1}) in
  let edge2 = Vert({row='a'; col=2}) in
  let edge3 = Hor ({row='b'; col=2}) in
  let edge4 = Hor ({row='c'; col=2}) in
  
  let edges = [(edge1, player1); (edge2, player2); (edge3, player1); (edge4, player2)] in
  let bd = {
    points = coords;
    edges = [];
    boxes = [];
    c_edges = edges;
    c_boxes = []
  } in
  assert (bd.c_boxes = []);
  (* Printf.printf "%s\n" (Printer.gap edge1.p1 edge1.p2 edges); *)
  Printf.printf "%s\n" (Printer.build_horiz bd 'a');
  Printf.printf "%s\n" (Printer.build_horiz bd 'b');
  Printf.printf "%s\n" (Printer.build_horiz bd 'c');
  

 

  Printf.printf "build_horiz tests complete\n"


TEST_UNIT "print a basic board" = 
  let player1 = {ai=(-1); id=1; score=0} in  (* green *)
  let player2 = {ai=(-1); id=2; score=0} in  (* red *)
  let coords = gen_coords 3 3 in

  let edge1 = Hor ({row='a'; col=1}) in
  let edge2 = Vert({row='a'; col=2}) in
  let edge3 = Hor ({row='b'; col=2}) in
  let edge4 = Hor ({row='c'; col=2}) in
  let edge5 = Vert({row='a'; col=1}) in
  let edge6 = Hor ({row='b'; col=1}) in
  
  let edges = [(edge1, player1); (edge2, player2); (edge3, player1); 
               (edge4, player2); (edge5, player1); (edge6, player2)] in
  let c_boxes = [(Box {row='a'; col=1}, player2)] in
  let bd = {
    points = coords;
    edges = [];
    boxes = [];
    c_edges = edges;
    c_boxes = c_boxes
  } in
  
 
  Printer.print_board bd;

  print_endline "";
  print_endline "";


  let coords2 = [{row='a'; col=1}; {row='a'; col=2}; {row='a'; col=4}; {row='a'; col=5};
                 {row='b'; col=1}; {row='b'; col=2}; {row='b'; col=4}; {row='b'; col=5};
                 {row='c'; col=4}; {row='c'; col=5}; {row='d'; col=4}; {row='d'; col=5}]
  in
  let edges2 = [(Vert {row='a'; col=1}, player1); (Vert {row='a'; col=4}, player2);
                (Hor  {row='a'; col=4}, player1); (Hor  {row='b'; col=4}, player2);
                (Vert {row='a'; col=5}, player1); (Vert {row='c'; col=4}, player2)]
  in
  let boxes2 = [(Box {row='a'; col=4}, player2)] in
  let board2 = {
    points = coords2;
    edges = [];
    boxes = [];
    c_edges = edges2;
    c_boxes = boxes2
  } in

  Printer.print_board board2;

  print_endline "";
  print_endline "";
 
  let coords3 = [{row='c'; col=8}; {row='c'; col=9}; {row='f'; col=7};
                 {row='d'; col=8}; {row='d'; col=9}] @ coords2 in
  let edges3 = edges2 @ [(Vert{row='c'; col=8}, player1);(Vert{row='c'; col=9}, player2)] in
  let board3 = { board2 with points=coords3; c_edges=edges3} in

  Printer.print_board board3;

  print_endline "";
  print_endline ""



TEST_UNIT "prints BoardGen boards" = 
  let player1 = {ai=(-1); id=1; score=0} in
  let player2 = {player1 with id=2} in

  let b = BoardGen.random_board 10 3 in
  let b' = {b with c_edges = 
    [(Hor {row='e'; col=4}, player1); (Vert {row = 'f'; col=5}, player2)]
  } in
  Printer.print_board b';

  print_endline "";
  print_endline "";
(*
  Printer.print_board (BoardGen.random_board 10 3);
  *)

TEST_UNIT "print_state" =
  let player1 = {ai=(-1); id=1; score=0} in  (* green *)
  let player2 = {ai=(1); id=2; score=0} in  (* red *)

  let coords2 = [{row='a'; col=1}; {row='a'; col=2}; {row='a'; col=4}; {row='a'; col=5};
                 {row='b'; col=1}; {row='b'; col=2}; {row='b'; col=4}; {row='b'; col=5};
                 {row='c'; col=4}; {row='c'; col=5}; {row='d'; col=4}; {row='d'; col=5}]
  in
  let edges2 = [(Vert {row='a'; col=1}, player1); (Vert {row='a'; col=4}, player2);
                (Hor  {row='a'; col=4}, player1); (Hor  {row='b'; col=4}, player2);
                (Vert {row='a'; col=5}, player1); (Vert {row='c'; col=4}, player2)]
  in
  let boxes2 = [(Box {row='a'; col=4}, player2)] in
  let board2 = {
    points = coords2;
    edges = [];
    boxes = [];
    c_edges = edges2;
    c_boxes = boxes2
  } in

  let coords3 = [{row='c'; col=8}; {row='c'; col=9};
                 {row='d'; col=8}; {row='d'; col=9}] @ coords2 in
  let edges3 = edges2 @ [(Vert{row='c'; col=8}, player1);(Vert{row='c'; col=9}, player2)] in
  let board3 = { board2 with points=coords3; c_edges=edges3} in

  let state = {
    players = [player1; player2];
    board = board3;
    to_move = player1;
    gameOver = false
  } in

  Printer.print_state state;

  print_endline "";
  print_endline ""


