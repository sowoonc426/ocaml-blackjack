open OUnit2
open Blackjack
open Blackjack.Card

let make_equality_test name actual expected printer =
  name >:: fun _ -> assert_equal expected actual ~printer

let string_of_list list = String.concat "," list
let string_of_cards list = String.concat " " (List.map Card.string list)

let string_contains search target =
  let re = Str.regexp_string target in
  try
    ignore (Str.search_forward re search 0);
    true
  with Not_found -> false

module Logger = struct
  type t = string list ref

  let empty () : t = ref []
  let append t str = t := !t @ [ str ]

  let collect t =
    let logs = !t in
    let logs = List.flatten (List.map (String.split_on_char '\n') logs) in
    let logs = List.map String.trim logs in
    let logs = List.filter (fun s -> String.length s != 0) logs in
    let logs = List.filter (fun s -> not (string_contains s "---")) logs in
    logs
end

module FakeInput = struct
  type t = string list ref

  exception Empty

  let empty () : t = ref []
  let init lst : t = ref lst

  let take (t : t) () =
    match !t with
    | [] -> raise Empty
    | hd :: tl ->
        t := tl;
        hd
end

(* Helper functions for fake input and fake output. *)
(* Fake input is a list of string inputs, where each item simulates a user's
   typed response. *)
let run_with_input input func =
  let fake_input = FakeInput.init input in
  func (FakeInput.take fake_input)

(* Output is collected where each output line is an item of the output list.
   Returns the function output and the list of logs *)
let run_collect_output func =
  let logger = Logger.empty () in
  let result = func (Logger.append logger) in
  let logs = Logger.collect logger in
  (result, logs)

(* In a function with both input and output, it is assumed that the input
   function is desired first. *)
let run_input_output input func =
  let fake_input = FakeInput.init input in
  let logger = Logger.empty () in
  let result = func (FakeInput.take fake_input) (Logger.append logger) in
  let logs = Logger.collect logger in
  (result, logs)

(* Same as [run_input_output], but automatically inputs a delay of 0. as the
   third parameter. *)
let run_input_output_delay input func =
  let new_func input output = func input output 0. in
  run_input_output input new_func

type output_piece =
  | RoundStart of int * int (* round number and player's money *)
  | DealerInit of Card.t
  | InputWager
  | HandTurn of int
  | PlayerHand of Card.t list
  | Options of {
      surrender : bool;
      specials : bool;
      split : bool;
    }
    (* whether split is included *)
  | Hit of Card.t
  | DoubleDown of Card.t * int
  | Split of
      int * Card.t * int * Card.t * int (* (hand_number, card) * 2, wager *)
  | ShadowStart
  | ShadowInput
  | Shadow of Card.t * Card.t (* shadow_card, hit_card *)
  | Surrender of int
  | Flip of int
  | BustTurnEnd
  | BlackjackTurnEnd
  | DealerTurn
  | DealerHand of Card.t list
  | DealerHit of Card.t
  | DealerEnd
  | Result of int * Card.t list * int * int
  | WinResult
  | BlackjackResult
  | LossResult
  | BustResult
  | TieResult
  | Loss of int
  | Win of int
  | GameOver of int
  | NoHandsRemaining

let convert_piece piece =
  match piece with
  | RoundStart (round_num, money) ->
      [ "ROUND " ^ string_of_int round_num; "You have $" ^ string_of_int money ]
  | DealerInit card -> [ "Dealer has: " ^ Card.string card ^ " ██" ]
  | InputWager -> [ "Input wager: $" ]
  | HandTurn x -> [ "Hand #" ^ string_of_int x ^ "'s Turn:" ]
  | PlayerHand cards -> [ string_of_cards cards ]
  | Options { surrender; specials; split } ->
      let options = ref [ "(H)it"; "(S)tand" ] in
      if surrender then options := !options @ [ "S(u)rrender" ];
      if specials then
        options := !options @ [ "$(D)ouble down$"; "$(F)lip$"; "$Sh(a)dow$" ];
      if split then options := !options @ [ "$S(p)lit$" ];
      [ "Do you want to"; "[ " ^ String.concat " / " !options ^ " ]?" ]
  | Hit card -> [ "Hit!"; "Got: " ^ Card.string card ]
  | DoubleDown (card, wager) ->
      [
        "Doubled down!";
        "Hit:";
        Card.string card;
        "Wagered an additional $" ^ string_of_int wager ^ ".";
      ]
  | Split (hand_num1, card1, hand_num2, card2, wager) ->
      [
        "Deck split!";
        "Hand #" ^ string_of_int hand_num1 ^ " hit:";
        Card.string card1;
        "Hand #" ^ string_of_int hand_num2 ^ " hit:";
        Card.string card2;
        "Wagered an additional $" ^ string_of_int wager ^ ".";
      ]
  | ShadowStart -> [ "Shadowing..." ]
  | ShadowInput -> [ "Shadow which card into a new hand? [ 1 / 2 ]" ]
  | Shadow (shadow_card, hit_card) ->
      [
        Card.string shadow_card ^ " shadowed!";
        "Hit:";
        Card.string hit_card;
        "New hand:";
        Card.string hit_card ^ " " ^ Card.string shadow_card;
      ]
  | Surrender x ->
      [
        "You surrendered.";
        "This hand is removed from the game. You receive $" ^ string_of_int x
        ^ " back.";
      ]
  | Flip x ->
      [
        "Flipping...";
        "Flip which card with the dealer's face-up card? [ 1 / 2 ]";
        "Flipped!";
        "Wagered an additional $" ^ string_of_int x ^ ".";
      ]
  | BustTurnEnd -> [ "Bust! Your turn is over." ]
  | BlackjackTurnEnd -> [ "Blackjack! You win, and your turn is over." ]
  | DealerTurn -> [ "Dealer's Turn:" ]
  | DealerHand cards -> [ "Dealer has:"; string_of_cards cards ]
  | DealerHit card -> [ "Hit: " ^ Card.string card ]
  | DealerEnd -> [ "End round."; "RESULTS" ]
  | Result (hand_num, cards, hand_val, dealer_val) ->
      [
        "Hand #" ^ string_of_int hand_num ^ ":";
        string_of_cards cards;
        "You have " ^ string_of_int hand_val ^ ", dealer has "
        ^ string_of_int dealer_val ^ ", so:";
      ]
  | WinResult -> [ "You win!" ]
  | BlackjackResult -> [ "Blackjack! You win!" ]
  | LossResult -> [ "You lose ):" ]
  | BustResult -> [ "Bust! You lose ):" ]
  | TieResult -> [ "Tie!" ]
  | Loss x -> [ "You lost $" ^ string_of_int x ]
  | Win x -> [ "You won $" ^ string_of_int x ]
  | GameOver x ->
      [ "GAME OVER"; "You ran out of money. Final amount: $" ^ string_of_int x ]
  | NoHandsRemaining -> [ "Round over. No hands remaining." ]

let rec build_output pieces =
  match pieces with
  | [] -> []
  | h :: t ->
      let new_strings = convert_piece h in
      new_strings @ build_output t

let dummy_player () = Player.init "dummy" 1000

let pad_list l1 l2 =
  let len1 = List.length l1 in
  let len2 = List.length l2 in
  if len1 >= len2 then l1
  else
    let padding = List.init (len2 - len1) (fun _ -> "") in
    l1 @ padding

let combine_string_lists l1_shorter l2 =
  let l1 = pad_list l1_shorter l2 in
  List.map2 (fun s1 s2 -> Printf.sprintf "%-40s%-40s" s2 s1) l1 l2

let test_game_loop name cards input output_pieces =
  let deck = Deck.from_cards cards in
  let player = dummy_player () in
  let _, logs =
    run_input_output_delay input (Game.game_loop ~recursive:false 1 player deck)
  in
  let test_name = "Game loop: " ^ name in
  let expected_output = build_output output_pieces in
  if not (logs = expected_output) then (
    let logs_with_expectations = combine_string_lists expected_output logs in
    print_endline name;
    print_endline
      ("logs / expectation:\n\t" ^ String.concat "\n\t" logs_with_expectations));
  make_equality_test test_name logs expected_output string_of_list

let game_loop_tests =
  [
    test_game_loop "Hit until bust"
      [
        make Spades Ace;
        make Diamonds Ace;
        make Hearts King;
        make Hearts King;
        make Hearts King;
        make Hearts King;
        make Hearts King;
      ]
      [ "500"; "h"; "h"; "h" ]
      [
        RoundStart (1, 1000);
        DealerInit (make Diamonds Ace);
        InputWager;
        HandTurn 1;
        PlayerHand [ make Hearts King; make Hearts King ];
        Options { surrender = true; split = true; specials = true };
        Hit (make Hearts King);
        PlayerHand [ make Hearts King; make Hearts King; make Hearts King ];
        BustTurnEnd;
        DealerTurn;
        DealerHand [ make Diamonds Ace; make Spades Ace ];
        DealerHit (make Hearts King);
        DealerHand [ make Hearts King; make Diamonds Ace; make Spades Ace ];
        DealerHit (make Hearts King);
        DealerHand
          [
            make Hearts King;
            make Hearts King;
            make Diamonds Ace;
            make Spades Ace;
          ];
        DealerEnd;
        Result
          (1, [ make Hearts King; make Hearts King; make Hearts King ], 30, 22);
        BustResult;
        Loss 500;
      ];
    test_game_loop "Dealer Blackjack"
      [
        make Spades King; make Diamonds Ace; make Hearts King; make Hearts King;
      ]
      [ "p"; "500"; "~"; "s" ]
      [
        RoundStart (1, 1000);
        DealerInit (make Diamonds Ace);
        InputWager;
        InputWager;
        HandTurn 1;
        PlayerHand [ make Hearts King; make Hearts King ];
        Options { surrender = true; specials = true; split = true };
        PlayerHand [ make Hearts King; make Hearts King ];
        Options { surrender = true; specials = true; split = true };
        DealerTurn;
        DealerHand [ make Diamonds Ace; make Spades King ];
        DealerEnd;
        Result (1, [ make Hearts King; make Hearts King ], 20, 21);
        LossResult;
        Loss 500;
      ];
    test_game_loop "Double down immediately"
      [
        make Spades King;
        make Hearts (Number 10);
        make Hearts Queen;
        make Hearts (Number 5);
        make Hearts (Number 6);
      ]
      [ "500"; "d" ]
      [
        RoundStart (1, 1000);
        DealerInit (make Hearts (Number 10));
        InputWager;
        HandTurn 1;
        PlayerHand [ make Hearts (Number 5); make Hearts Queen ];
        Options { surrender = true; specials = true; split = false };
        DoubleDown (make Hearts (Number 6), 500);
        PlayerHand
          [ make Hearts (Number 6); make Hearts (Number 5); make Hearts Queen ];
        DealerTurn;
        DealerHand [ make Hearts (Number 10); make Spades King ];
        DealerEnd;
        Result
          ( 1,
            [
              make Hearts (Number 6); make Hearts (Number 5); make Hearts Queen;
            ],
            21,
            20 );
        BlackjackResult;
        Win 1500;
      ];
    test_game_loop "Split once"
      [
        make Spades King;
        make Hearts Queen;
        make Hearts Ace;
        make Spades Ace;
        make Spades Ace;
        make Clubs Queen;
      ]
      [ "500"; "p"; "s"; "s" ]
      [
        RoundStart (1, 1000);
        DealerInit (make Hearts Queen);
        InputWager;
        HandTurn 1;
        PlayerHand [ make Spades Ace; make Hearts Ace ];
        Options { surrender = true; specials = true; split = true };
        Split (1, make Spades Ace, 2, make Clubs Queen, 500);
        PlayerHand [ make Spades Ace; make Spades Ace ];
        Options { surrender = true; specials = false; split = false };
        HandTurn 2;
        PlayerHand [ make Clubs Queen; make Hearts Ace ];
        BlackjackTurnEnd;
        DealerTurn;
        DealerHand [ make Hearts Queen; make Spades King ];
        DealerEnd;
        Result (1, [ make Spades Ace; make Spades Ace ], 12, 20);
        LossResult;
        Loss 500;
        Result (2, [ make Clubs Queen; make Hearts Ace ], 21, 20);
        BlackjackResult;
        Win 750;
      ];
    test_game_loop "Shadowing"
      [
        make Spades King;
        make Hearts (Number 9);
        make Diamonds Jack;
        make Clubs Jack;
        make Spades (Number 9);
      ]
      [ "500"; "a"; "0"; "f"; "1"; "s"; "s" ]
      [
        RoundStart (1, 1000);
        DealerInit (make Hearts (Number 9));
        InputWager;
        HandTurn 1;
        PlayerHand [ make Clubs Jack; make Diamonds Jack ];
        Options { surrender = true; specials = true; split = true };
        ShadowStart;
        ShadowInput;
        ShadowInput;
        ShadowInput;
        Shadow (make Clubs Jack, make Spades (Number 9));
        PlayerHand [ make Clubs Jack; make Diamonds Jack ];
        Options { surrender = false; specials = false; split = false };
        HandTurn 2;
        PlayerHand [ make Spades (Number 9); make Clubs Jack ];
        Options { surrender = true; specials = false; split = false };
        DealerTurn;
        DealerHand [ make Hearts (Number 9); make Spades King ];
        DealerEnd;
        Result (1, [ make Clubs Jack; make Diamonds Jack ], 20, 19);
        WinResult;
        Win 500;
        Result (2, [ make Spades (Number 9); make Clubs Jack ], 19, 19);
        TieResult;
      ];
    test_game_loop "Surrender option"
      [
        make Spades King; make Diamonds Ace; make Hearts King; make Hearts King;
      ]
      [ "500"; "u" ]
      [
        RoundStart (1, 1000);
        DealerInit (make Diamonds Ace);
        InputWager;
        HandTurn 1;
        PlayerHand [ make Hearts King; make Hearts King ];
        Options { surrender = true; specials = true; split = true };
        Surrender 250;
        NoHandsRemaining;
      ];
    test_game_loop "Flip"
      [
        make Hearts King; make Spades King; make Diamonds King; make Clubs Queen;
      ]
      [ "500"; "f"; "2"; "s" ]
      [
        RoundStart (1, 1000);
        DealerInit (make Spades King);
        InputWager;
        HandTurn 1;
        PlayerHand [ make Clubs Queen; make Diamonds King ];
        Options { surrender = true; specials = true; split = false };
        Flip 500;
        PlayerHand [ make Clubs Queen; make Spades King ];
        Options { surrender = false; specials = false; split = false };
        DealerTurn;
        DealerHand [ make Diamonds King; make Hearts King ];
        DealerEnd;
        Result (1, [ make Clubs Queen; make Spades King ], 20, 20);
        TieResult;
      ];
  ]

let test_suite = "test_game suite" >::: game_loop_tests
let _ = run_test_tt_main test_suite
