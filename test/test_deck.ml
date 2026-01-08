open OUnit2
open Blackjack

let make_equality_test name actual expected printer =
  name >:: fun _ -> assert_equal expected actual ~printer

let make_failure_test name expected func =
  name >:: fun _ -> assert_raises expected func

let string_of_list list = String.concat "," list

let make_list_test name actual expected =
  let actual_sorted = List.sort String.compare actual in
  let expected_sorted = List.sort String.compare expected in
  make_equality_test name actual_sorted expected_sorted string_of_list

let rec make_cards_list deck =
  if Deck.is_empty deck then []
  else
    let next_card = Deck.next_card deck in
    next_card :: make_cards_list deck

let ensure_deck_contains_all name deck expected_list =
  let cards = make_cards_list deck in
  let cards_list = List.map Card.string cards in
  make_list_test name cards_list expected_list

(* Ensures drawing only fails with an empty deck *)
let empty_draw_fails _ =
  let deck = Deck.init () in
  for _ = 1 to List.length Card.all_possibilities do
    ignore (Deck.next_card deck)
  done;
  make_failure_test "Ensure drawing from an empty deck fails." Deck.Empty
    (fun () -> Deck.next_card deck)

let test_deck_contents =
  [
    ensure_deck_contains_all "all possibilities returns all cards"
      (Deck.init ()) Card.all_possibilities_string;
    empty_draw_fails ();
  ]

(* Ensures shuffled deck is in a different order *)
let test_shuffle_changes_order _ =
  let original_deck = Deck.init () in
  let deck_copy = Deck.init () in
  Deck.shuffle deck_copy;
  let shuffled_cards = make_cards_list deck_copy in
  let original_cards = make_cards_list original_deck in
  original_cards <> shuffled_cards

(* Ensures shuffled deck contains the same cards *)
let test_shuffle_deck_contents _ =
  let original_deck = Deck.init () in
  let deck_copy = Deck.init () in
  let deck_copy = Deck.shuffle_deck deck_copy in
  let shuffled_cards = List.map Card.string (make_cards_list deck_copy) in
  let original_cards = List.map Card.string (make_cards_list original_deck) in
  let is_same_content =
    List.sort String.compare shuffled_cards
    = List.sort String.compare original_cards
  in
  is_same_content

let shuffle_order_tests =
  QCheck2.Test.make ~count:10 ~name:"Shuffle order different tests"
    QCheck2.Gen.unit test_shuffle_changes_order

let shuffle_contents_tests =
  QCheck2.Test.make ~count:10 ~name:"Shuffle content correctness tests"
    QCheck2.Gen.unit test_shuffle_deck_contents

let qcheck_tests = [ shuffle_order_tests; shuffle_contents_tests ]
let qcheck_ounit_tests = List.map QCheck_runner.to_ounit2_test qcheck_tests
let test_suite = "deck test suite" >::: test_deck_contents @ qcheck_ounit_tests
let _ = run_test_tt_main test_suite
