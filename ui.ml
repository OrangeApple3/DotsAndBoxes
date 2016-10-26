open Game

(* Test code *)
let gb = {points = [{row='a';col=1};{row='a';col=2};{row='b';col=1};
{row='b';col=2};{row='c';col=1}]; edges = [Hor({row='a';col=1});
Hor({row='b';col=1}); Vert({row='a';col=1});Vert({row='a';col=2});Vert({row='b';col=1})];
boxes = [Box({row='a';col=1})]; c_edges = []; c_boxes = []}

let p1 = {ai = 0; id = 1; score = 0}
let p2 = {ai = 0; id = 2; score = 0}
let p3 = {ai = 0; id = 3; score = 0}
let p4 = {ai = 0; id = 4; score = 0}
let gs = {players = [p1;p2;p3;p4]; board = gb; turn = 1; to_move = p1; gameOver = false}

type command = Help | ScoreChk | Exit | Move of Game.move

(* Prints some standardized help text *)
let help g =
  print_endline
  "\nValid commands consist of either a valid move, or one of these three
additional commands:";
  print_endline
    "\t'help':        Displays this screen";
  print_endline
    "\t'score':       Displays each player's score";
  print_endline
    "\t'turn':        Displays the current turn number";
  print_endline
    "\t'exit':        Exits the current game";
  print_endline
  "\nValid moves consist of two coordinates, inputted one at a time,
that a) exist on the board, b) are adjacent, and c) define an empty edge.\n";
  print_endline
  "Coordinates are themselves represented by a y-coordinate (denoted by a
letter, and an x-coordinate (denoted by a number) e.g., A4, B6 or E2\n";
  print_endline
  "Therefore, a valid move may involve inputting a4, and then a5, or perhaps
a4, and then b4, provided both those edges are free and on the board.";
  g

(* Prints the score of an individual player; helper function of score_check *)
let individual_score (p:player) =
  print_string "\nPlayer ";
  print_int (p.id);
  print_string " Score: ";
  print_int p.score;
  print_string "\n"

(* Prints the scores of every player *)
let score_check g =
  let rec score_check2 p =
    match p with
    | [] -> ()
    | hd::tl -> (individual_score hd; score_check2 tl) in
  score_check2 g.players; g

let turn g =
  print_string "\nTurn: ";
  print_int (g.turn);
  print_string "\n";
  g

(* Ends the game, for now - should it go to the main menu? *)
let quit () =
  print_endline "\nThanks for playing!\n";
  exit 0

(* Simple list member function *)
let rec mem x s =
  match s with
  | [] -> false
  | hd::tl -> (x = hd) || mem x tl

(* Turns user input to a move of coord*coord - syntactical checking pending,
 * and returns a new gamestate w/ that move using Game.update_gs *)
let move_parser g s =
  let c1 = {row = s.[0]; col = int_of_string (Bytes.sub s 1 ((Bytes.length s) -1))} in
  if (mem c1 g.board.points) then
    (print_string "\n2nd Coordinate? ";
    let inp = read_line () in
    let input = String.lowercase inp in
    let c2 = {row = input.[0]; col = int_of_string (Bytes.sub input 1 ((Bytes.length input)-1))} in
    if (mem c2 g.board.points) && Game.validate g.board (c1, c2) then
      Game.update_gs g (c1, c2) g.to_move
    else
      (print_string "\nInvalid move.";
      g
    )
  )
  else
    (print_string "\nInvalid move.";
    g
  )

  (* Ignore this block below for now - it is most likely outdated *)

  (*if (mem c1 g.board.points) then
    print_string "2nd Coordinate? ";
    let inp = read_line () in
    let input = String.lowercase inp in
    let c2 = {row = inp.[0]; col = int_of_string (Bytes.sub inp 1 ((Bytes.length inp)-1))} in
    if (mem c2 g.board.points) then
      (c1, c2)*)

(* Takes user input and directs them to the proper function *)
let take_input g =
  print_string "\nPlayer ";
  print_int (g.to_move.id);
  print_string " >>> ";
  let inp = read_line () in
  let input = String.lowercase inp in
    match input with
    | "help" -> help g
    | "turn" -> turn g
    | "exit" | "quit" -> quit ()
    | "score_check" | "score" -> score_check g
    | x -> move_parser g x

(* Starts the prompting *)
let rec prompt g =
  prompt (take_input g)

(* This is a main method - for testing purposes only *)
let () =
  (print_endline "TEST MAIN MUST REPLACE";
    ignore (prompt gs);
    ()
  )

let rec player_no () =
  let () = print_endline "Select the number of players (2-4)" in
  let () = print_string "\n>>>" in
  let inp = read_line () in
  let input = String.lowercase inp in
  match input with
  | "2" -> 2
  | "3" -> 3
  | "4" -> 4
  | _ -> (let () = print_endline "Invalid number of players" in
          player_no ())

let rec player_type n =
  let str = "Is Player " ^ (string_of_int n) ^ " Human or AI" in
  let () = print_endline str in
  let inp = read_line () in
  let input = String.lowecase inp in
  match input with
  | "human" -> -1
  | "ai" -> ai_diff
  | _ -> (let () = print_endline "Invalid input" in
          player_type n)

let rec ai_diff () =
  let () = print_endline "Select AI Difficulty (integer >= 0)" in
  let inp = read_line () in
  let input = String.lowercase inp in
  try
    let x = int_of_string input in
    let _ = assert (x >= 0) in
    x
  with
    _ -> "Invalid Difficulty"

let rec board_type () =
  let () = print_endline "Select STANDARD or RANDOM board" in
  let inp = read_line () in
  let input = String.lowercase inp in
  match input with
  | "standard" -> (let d1 = stand_board true in
                  let d2 = stand_board false in
                  (d1,d2))
  | "random" -> (let d = rand_board () in
                (0,d))
  | _ -> (let () = print_endline "Invalid input" in
          player_type n)

let rec stand_board bol =
  let () = match bol with
           | true -> print_endline "Select board height (integer > 0)"
           | false -> print_endline "Select board width (integer > 0)"
  let inp = read_line () in
  let input = String.lowercase inp in
  try
    let x = int_of_string input in
    let _ = assert (x > 0) in
    x
  with
    _ -> (let () = print_endline "Invalid dimension" in
         stand_board bol)

let rec rand_board () =
  let () = print_endline "Select randomness density (integer > 0)" in
  let inp = read_line () in
  let input = String.lowercase inp in
  try
     let x = int_of_string input in
     let _ = assert (x > 0) in
     x
  with
  | _ -> (let () = print_endline "Invalid randomness density" in
         rand_board ())
