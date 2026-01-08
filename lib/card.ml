type suite =
  | Spades
  | Hearts
  | Clubs
  | Diamonds

type value =
  | Number of int
  | Ace
  | Jack
  | Queen
  | King

type t = {
  suite : suite;
  value : value;
}

let make suite value : t =
  match value with
  | Number num ->
      if num < 1 || num > 10 then failwith "Invalid number"
      else { suite; value }
  | _ -> { suite; value }

let value card =
  match card.value with
  | Number num -> num
  | Jack -> 10
  | Queen -> 10
  | King -> 10
  | Ace -> 11

let value_cards cards =
  let aces, _ = List.partition (fun card -> card.value = Ace) cards in
  let total = List.fold_left (fun acc card -> acc + value card) 0 cards in
  let rec adjust_for_aces total num_aces =
    if total > 21 && num_aces > 0 then
      adjust_for_aces (total - 10) (num_aces - 1)
    else total
  in
  adjust_for_aces total (List.length aces)

let contains_high_ace cards =
  let value = value_cards cards in
  let aces, others = List.partition (fun card -> card.value = Ace) cards in
  let value_no_aces = value_cards others in
  if value_no_aces + List.length aces = value then false else true

let string_of_suite t =
  match t.suite with
  | Spades -> "♠"
  | Hearts -> "♥"
  | Diamonds -> "♦"
  | Clubs -> "♣"

let string_of_value t =
  match t.value with
  | Number num -> string_of_int num
  | Ace -> "A"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"

let string t = string_of_value t ^ string_of_suite t

let all_value_possibilities suite =
  [
    make suite Ace;
    make suite (Number 2);
    make suite (Number 3);
    make suite (Number 4);
    make suite (Number 5);
    make suite (Number 6);
    make suite (Number 7);
    make suite (Number 8);
    make suite (Number 9);
    make suite (Number 10);
    make suite Jack;
    make suite Queen;
    make suite King;
  ]

let all_possibilities =
  all_value_possibilities Spades
  @ all_value_possibilities Hearts
  @ all_value_possibilities Clubs
  @ all_value_possibilities Diamonds

let all_possibilities_string = String.split_on_char '\n' Cards.cards
