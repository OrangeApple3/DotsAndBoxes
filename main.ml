open Game

type command = Help | Quit | Hint | Move of move

(* [read_file f] returns the contents of file f as a string *)
let read_file f = 
  let in_chan = open_in f in
  let n = in_channel_length in_chan in
  let s = Bytes.create n in
  really_input in_chan s 0 n;
  close_in in_chan;
  s

let title_screen =
  read_file "title.txt"

let clear () = Printf.printf "%c[2J" (char_of_int 27)

(* Prints some standardized help text *)
let help () =
  print_endline("     _       _                  _");
  print_endline("    | |    _| |_               | |");
  print_endline("  __| | __|_   _|__      _     | |__   ___ _  _ ___  ___");
  print_endline(" / _  |/ _ \\| |/ __\\   _| |_   |  _ \\ / _ \\ \\/ //_\\\\/ __\\");
  print_endline("| (_| | (_) | |\\__ \\  |_   _|  | |_) | (_) >  < (___\\__ \\");
  print_endline(" \\____|\\___/|_|____/    |_|    |____/ \\___/_/\\_\\___|____/");

  print_endline
  "\nValid commands consist of either a valid move, or one of these three
additional commands:";
  print_endline
    "\t'help' or '?':           Displays this screen";
  print_endline
    "\t'exit' or 'quit':        Exits the current game";
  print_endline
    "\t'hint':                  Suggests a move. Suggested moves are
                                 printed in white on a new board";
  print_endline
  "\nMoves consist of two coordinates, separated by a space.
Valid moves consist of two adjacent coordinates which do not already have
an edge between them.\n";
  print_endline
  "Coordinates are themselves represented by a y-coordinate (denoted by a
letter, and an x-coordinate (denoted by a number) e.g., A4, B6 or E2\n";
  print_endline
  "Therefore, a valid move may involve inputting 'a4 a5', or perhaps
  'b4 a4' provided both those edges are free and on the board.\n";
  print_endline
  "\n   Press enter to leave this screen  \n";
  let line = input_line stdin in
  ignore (line) 

(* Ends the game, for now - should it go to the main menu? *)
let quit () =
  print_endline "\nThanks for playing!\n";
  exit 0

(* gives the player a hint about what the ai would do next *)
let hint gs = 
  let move = Ai.best_move gs 2 in
  let edge = Game.edge_of_move move in
  let board' = {gs.board with c_edges = (edge, {ai=2; id=7; score=0})::gs.board.c_edges} in
  let gs' = {gs with board = board'} in
  print_string "\n Hint board: ";
  Printer.print_state gs';
  let c1 = (String.make 1 (fst move).row)^(string_of_int (fst move).col) in
  let c2 = (String.make 1 (snd move).row)^(string_of_int (snd move).col) in
  Printf.printf "\n\nSuggested move is: %s %s\n" c1 c2;
  print_string "Make your move:"

let rec player_no () =
  let () = print_endline "\nSelect the number of players (2-4)" in
  let () = print_string ">>> " in
  let inp = read_line () in
  let input = String.lowercase inp in
  match input with
  | "2" -> 2
  | "3" -> 3
  | "4" -> 4
  | _ -> (let () = print_endline "Invalid number of players" in
          player_no ())

(* asks the user for how difficult they would like an ai to be *)
let rec ai_diff () =
  let () = print_endline "\nSelect AI Difficulty (0 1 or 2)" in
  let () = print_string ">>> " in
  let inp = read_line () in
  let input = String.lowercase inp in
  try
    let x = int_of_string input in
    let _ = assert (x >= 0) in
    x
  with
    _ -> (let () = print_endline "Invalid Difficulty" in
          ai_diff ())


(* prompts user for information about whether a given player is human or ai *)
let rec player_type n =
  let str = "\nIs Player " ^ (string_of_int n) ^ " Human or AI?" in
  let () = print_endline str in
  let () = print_string ">>> " in
  let inp = read_line () in
  let input = String.lowercase inp in
  match input with
  | "human" | "h" | "hum" -> -1
  | "ai" -> ai_diff ()
  | _ -> (let () = print_endline "Invalid input" in
          player_type n)


(* prompts the user to enter parameters for standard board generation *)
let rec stand_board bol =
  let s = match bol with
        | true -> 
           "Select board height (integer > 0) values larger than 16 not recommended!"
        | false -> 
           "Select board width (integer > 0)  values larger than 16 not recommended! "
  in
  let () = print_endline s in
  let inp = read_line () in
  let input = String.lowercase inp in
  try
    let x = int_of_string input in
    let _ = assert (x > 0) in
    x
  with
    _ -> (let () = print_endline "Invalid dimension" in
         stand_board bol)

(* prompts the user to enter parameter for random board generation *)
let rec rand_board () =
  let () = print_endline "\nSelect randomness density (integer > 0) (lower is sparser)" in
  let () = print_string ">>> " in
  let inp = read_line () in
  let input = String.lowercase inp in
  try
     let x = int_of_string input in
     let _ = assert (x > 0) in
     x
  with
  | _ -> (let () = print_endline "Invalid randomness density" in
         rand_board ())

(* asks the user whether they would like a standard or a random board *)
let rec board_type () =
  let () = print_endline "\nSelect STANDARD or RANDOM board" in
  let () = print_string ">>> " in
  let inp = read_line () in
  let input = String.lowercase inp in
  match input with
  | "standard" | "s" | "st" | "std"  -> 
            (let d1 = stand_board true in
             let d2 = stand_board false in
             (d1,d2))
  | "random" | "r" | "rand"  -> 
            (let d = rand_board () in
             (0,d))
  | _ -> (let () = print_endline "Invalid input" in
          board_type ())

(* Precondition: total is less than n *)
let rec player_list n total =
  if n-1 = total then
    []
  else
    let t = player_type n in
    (make_player t n)::(player_list (n+1) total)


(* tries to convert an input string to a coordinate which may or may not be valid *)
let input_to_coord str =
  let r = str.[0] in
  try
    let x = String.sub str 1 ((String.length str) - 1) in
    let c = int_of_string x in
    Some {row = r; col = c}
  with
    _ -> None
  
(* Converts a list of words into a move, if possible.
 * Precondition: l has length 2 *)
let inputlist_to_move l =
  let c1 = input_to_coord (List.nth l 0) in
  let c2 = input_to_coord (List.nth l 1) in
  match (c1,c2) with
  | (None,_) -> None
  | (_,None) -> None
  | (Some x,Some y) -> 
      if (int_of_char x.row > int_of_char y.row || x.col > y.col) then Some (y,x)
      else Some(x,y)

(* [input_to_move inp] takes in a string of input which might represent
 * a move, and trys to return a command containing the corresponding move.
 * 
 *
 * For example, input_to_move "b4 c4" returns 
 *   Move ({row='b'; col=4}, {row='c'; col=4})
 *)
let rec input_to_move (input: string) gs : command = 
  let inp = String.trim input in
  let space_re = Str.regexp "[ \t]+" in
  let split_lst = Str.split space_re inp in
  try
    let () = assert ((List.length split_lst) = 2) in
    match inputlist_to_move split_lst with
    | None -> let () = assert false in Quit
    | Some m -> 
        let () = assert (Game.validate gs.board m) in
        Move m
  with
    _ -> 
      let () = print_endline "Invalid move, please enter a new move or command." in
      prompt_and_parse gs
   


  (*
  let s_coord1 = String.sub inp 0 2 in
  let s_coord2 = String.sub inp ((String.length inp)-2) 2 in
  
  let string_to_coord s = {row = s.[0]; col = int_of_char s.[1]} in

  let c1 = string_to_coord s_coord1 in
  let c2 = string_to_coord s_coord2 in
  let () = Printf.printf "c1 is %s\n" s_coord1 in
  let () = Printf.printf "c2 is %s\n" s_coord2 in

  if ((*List.mem c1 gs.board.points && List.mem c2 gs.board.points)
     && *) Game.validate gs.board (c1, c2))
  then Move (c1, c2)
  else 
    let () = print_endline "Invalid move, please enter a new move or command." in
    prompt_and_parse gs
    *)

(* promp the user for input, parse that input, and return a command
 * representative of that input *)
and prompt_and_parse gs : command = 
  Printf.printf "\nPlayer %i (? for help) >>> " (gs.to_move.id);
  let raw_inp = read_line () in
  let input = String.trim (String.lowercase raw_inp) in
  match input with
    | "?" | "help" | "h" -> Help
    | "quit" | "q" | "exit" | "q!" -> Quit
    | "hint" -> Hint
    | x -> input_to_move x gs

(* The main game loop. Prompts players for input or commands, runs those
 * commands, and updates the game state accordingly, then loops again *)
let rec game_loop gs : gameState = 
  let () = Printer.print_state gs in
  if gs.gameOver then
    let () = print_endline "The game is over!" in
    let () = match Game.winners gs with
      | []  -> Printf.printf "Uh oh, no winner?\n"
      | [x] -> Printf.printf "Player %i wins!!!\n" x.id
      | xs  -> 
          let rec string_of_winners lst so_far =
            match lst with
              | [] -> so_far
              | x::xs ->
                  string_of_winners xs so_far^(Printf.sprintf "%i" x.id)
          in
          Printf.printf "It's a tie between Players %s" (string_of_winners xs "")
    in exit 0

  else
    if gs.to_move.ai >= 0 then
      let move = Ai.best_move gs gs.to_move.ai in
      let gs' = Game.update_gs gs move gs.to_move in
      game_loop gs'
    else
      match prompt_and_parse gs with
        | Help -> help (); game_loop gs 
        | Quit -> let () = quit () in game_loop gs
        | Hint -> hint gs; game_loop gs
        | Move m -> 
            let gs' = Game.update_gs  gs  m  gs.to_move in
            game_loop gs'


(* The entry point of the game, which prints a title screen
 * then prompts the user to select game attributes, such as 
 * how many players and what kind of board *)
let main_menu () : gameState =
  clear ();
  Printf.printf "%s" title_screen;

  let num_players = player_no () in
  let plist = player_list 1 num_players in
  let (d1, d2) = board_type () in
  let b = match d1 with
          | 0 -> BoardGen.random_board 10 d2
          | _ -> BoardGen.standard_board d1 d2 
  in
  let g = Game.create_gs plist b in
  let () = print_endline "\n  BEGIN!" in
  game_loop g


let _ = main_menu ()
