open OUnit2
open Blackjack
open Card

let test_initialize_player _ =
  let player = Player.init "Alice" 1000 in
  assert_equal player.name "Alice";
  assert_equal player.hands (HandGroup.empty ())

(* Test for add_card_to_hand *)
let test_add_card_to_hand _ =
  let player = Player.init "Bob" 1000 in
  let card = make Hearts (Number 5) in
  Player.add_card_to_hand player card;
  assert_equal (HandGroup.curr_hand player.hands).cards [ card ]

(* Test for reset_hand *)
let test_reset_hand _ =
  let player = Player.init "Carol" 1000 in
  let card1 = make Spades (Number 8) in
  let card2 = make Clubs Queen in
  Player.add_card_to_hand player card1;
  Player.add_card_to_hand player card2;
  Player.reset player 0;
  assert_equal player.hands (HandGroup.empty ())

(* Test for calculate_hand_value *)
let test_calculate_hand_value _ =
  let card1 = make Diamonds (Number 2) in
  let card2 = make Clubs (Number 9) in
  let hand = Hand.init_unwagered [ card1; card2 ] in
  assert_equal (Hand.value hand) 11

let make_test_split player_name initial_hand expected_result : OUnit2.test =
  let test_name = "split for " ^ player_name in
  test_case (fun _ ->
      let player = Player.init player_name 1000 in
      List.iter (Player.add_card_to_hand player) initial_hand;
      let hand = HandGroup.curr_hand player.hands in
      let split_allowed = Player.can_split player hand in
      match expected_result with
      | Some (exp_hand1_cards, exp_hand2_cards) ->
          if not split_allowed then
            assert_failure (test_name ^ " - expected split allowed, but isn't.")
          else
            let exp_hand1 = Hand.init_unwagered exp_hand1_cards in
            let exp_hand2 = Hand.init_unwagered exp_hand2_cards in
            let _ = Player.split player hand in
            let hand1 = HandGroup.curr_hand player.hands in
            HandGroup.next_hand player.hands;
            let hand2 = HandGroup.curr_hand player.hands in
            assert_equal ~msg:(test_name ^ " - hand1") exp_hand1 hand1;
            assert_equal ~msg:(test_name ^ " - hand2") exp_hand2 hand2
      | None ->
          if split_allowed then
            assert_failure (test_name ^ " - expected split not allowed, but isn")
          else
            assert_raises (Failure "Hand is not allowed to split.") (fun () ->
                Player.split player hand))

let test_split =
  "Test split function"
  >::: [
         (* Split succeeds when both cards have the same value *)
         make_test_split "Alice"
           [ make Hearts (Number 8); make Diamonds (Number 8) ]
           (Some ([ make Diamonds (Number 8) ], [ make Hearts (Number 8) ]));
         (* Split fails because the cards have different values *)
         make_test_split "Bob"
           [ make Hearts (Number 8); make Diamonds King ]
           None;
         (* Split fails because too many cards in hand *)
         make_test_split "Candice"
           [ make Diamonds King; make Diamonds King; make Diamonds King ]
           None;
       ]

(* Combine all tests *)
let suite =
  "Player Tests"
  >::: [
         "test_initialize_player" >:: test_initialize_player;
         "test_add_card_to_hand" >:: test_add_card_to_hand;
         "test_reset_hand" >:: test_reset_hand;
         "test_calculate_hand_value" >:: test_calculate_hand_value;
         test_split;
         (* test_double_down; *)
       ]

(* Run the test suite *)
let _ = run_test_tt_main suite
