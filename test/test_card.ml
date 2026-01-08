open OUnit2
open Blackjack
open Blackjack.Card

let make_equality_test name actual expected printer =
  name >:: fun _ -> assert_equal expected actual ~printer

let make_failure_test name expected func =
  name >:: fun _ -> assert_raises expected func

let string_of_list list = String.concat "," list

let make_list_test name actual expected =
  let actual_sorted = List.sort String.compare actual in
  let expected_sorted = List.sort String.compare expected in
  make_equality_test name actual_sorted expected_sorted string_of_list

let make_card_list_test name actual_list expected_list =
  let card_list = List.map Card.string actual_list in
  make_list_test name card_list expected_list

let random_suite () : suite =
  match Random.int 4 with
  | 0 -> Spades
  | 1 -> Hearts
  | 2 -> Clubs
  | _ -> Diamonds

let random_face () : value =
  match Random.int 3 with
  | 0 -> Jack
  | 1 -> Queen
  | _ -> King

let cards_from_values values =
  List.map (fun value -> make (random_suite ()) value) values

let make_card_list_value_test values expected_value =
  let cards = cards_from_values values in
  let value = value_cards cards in
  let card_strings = List.map Card.string cards in
  let name =
    "value of cards "
    ^ String.concat "," card_strings
    ^ " is "
    ^ string_of_int expected_value
  in
  make_equality_test name value expected_value string_of_int

let make_contains_high_ace_test values expected_value =
  let cards = cards_from_values values in
  let contains_high_ace = Card.contains_high_ace cards in
  let card_strings = List.map Card.string cards in
  let name =
    "cards "
    ^ String.concat "," card_strings
    ^ (if expected_value then " contains" else " does not contain")
    ^ " high ace"
  in
  make_equality_test name contains_high_ace expected_value string_of_bool

let make_card_list_value_qcheck_test values expected_value =
  let cards = cards_from_values values in
  let value = value_cards cards in
  value = expected_value

let rec random_cards_that_add_to num =
  match num with
  | 0 -> []
  | _ ->
      let max_num = min num 10 in
      let next_num = Random.int max_num + 1 in
      Number next_num :: random_cards_that_add_to (num - next_num)

let random_cards_value_test num =
  let cards = random_cards_that_add_to num in
  make_card_list_value_qcheck_test cards num

let random_cards_value_test_add_big_ace num =
  let cards = Ace :: random_cards_that_add_to num in
  make_card_list_value_qcheck_test cards (num + 11)

let random_cards_value_test_add_small_ace num =
  let cards = Ace :: random_cards_that_add_to num in
  make_card_list_value_qcheck_test cards (num + 1)

let random_cards_value_test_add_face num =
  let cards = random_face () :: random_cards_that_add_to num in
  make_card_list_value_qcheck_test cards (num + 10)

let gen_bounded_int_under_11 = QCheck2.Gen.int_range 1 10
let gen_bounded_int_over_11 = QCheck2.Gen.int_range 11 50
let gen_bounded_int = QCheck2.Gen.int_range 1 50

let random_cards_test =
  QCheck2.Test.make ~count:500 ~name:"" gen_bounded_int random_cards_value_test

let big_aces_test =
  QCheck2.Test.make ~count:500 ~name:"" gen_bounded_int_under_11
    random_cards_value_test_add_big_ace

let small_aces_test =
  QCheck2.Test.make ~count:500 ~name:"" gen_bounded_int_over_11
    random_cards_value_test_add_small_ace

let face_cards_test =
  QCheck2.Test.make ~count:500 ~name:"" gen_bounded_int
    random_cards_value_test_add_face

let qcheck_tests =
  [ random_cards_test; big_aces_test; small_aces_test; face_cards_test ]

let qcheck_ounit_tests = List.map QCheck_runner.to_ounit2_test qcheck_tests

let cards_values_pairs =
  [
    ([ Ace; Ace ], 12);
    ([ Ace; Jack; Ace ], 12);
    ([ Number 10; Ace; Ace; Ace; Ace; Ace; Number 4 ], 19);
  ]

let test_card_values =
  List.map
    (fun pair -> make_card_list_value_test (fst pair) (snd pair))
    cards_values_pairs

let contains_high_ace_pairs =
  [ ([ Ace; Ace ], true); ([ Ace; Jack ], true); ([ Ace; Jack; Ace ], false) ]

let test_contains_high_ace =
  List.map
    (fun pair -> make_contains_high_ace_test (fst pair) (snd pair))
    contains_high_ace_pairs

let test_all_cards_exist =
  [
    make_card_list_test "all possibilities returns all cards"
      Card.all_possibilities Card.all_possibilities_string;
  ]

let make_invalid_card_tests nums =
  let make_invalid_card_test num =
    let name = "Card with number " ^ string_of_int num ^ " fails" in
    make_failure_test name (Failure "Invalid number") (fun () ->
        Card.make (random_suite ()) (Number num))
  in
  List.map make_invalid_card_test nums

let invalid_card_tests = make_invalid_card_tests [ -1; 11; 100 ]

let test_suite =
  "card test suite"
  >::: test_all_cards_exist @ test_card_values @ test_contains_high_ace
       @ qcheck_ounit_tests @ invalid_card_tests

let _ = run_test_tt_main test_suite
