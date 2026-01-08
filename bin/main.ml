open Blackjack

let print_string_flush string =
  print_string string;
  flush stdout
;;

let _ = Random.self_init () in

Game.start_game ~broken:false 1000 read_line print_string_flush 0.5
