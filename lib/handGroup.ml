type t = {
  mutable hands : Hand.t list;
  mutable currHand : int;
}

let empty () = { hands = [ Hand.empty_unwagered () ]; currHand = 0 }
let is_empty t = List.is_empty t.hands
let curr_hand t = List.nth t.hands t.currHand

let add_card_to_curr_hand t card =
  let c_hand = curr_hand t in
  Hand.add_card c_hand card

let next_hand t =
  let newHand = t.currHand + 1 in
  let rotatedNewHand = if newHand >= List.length t.hands then 0 else newHand in
  t.currHand <- rotatedNewHand

let increase_index_if_necessary t idx =
  if t.currHand > idx then t.currHand <- t.currHand + 1

(* inserts a new hand after an index current hand *)
let insert_hand t new_hand idx =
  let mappedHands =
    List.mapi (fun i h -> if i != idx then [ h ] else [ h; new_hand ]) t.hands
  in
  let new_hands = List.flatten mappedHands in
  t.hands <- new_hands;
  increase_index_if_necessary t idx

let reset t wager =
  t.hands <- [ Hand.empty wager ];
  t.currHand <- 0

let map_over_hands t func =
  let initial_hand = t.currHand in
  let rec map_helper () =
    let hand = curr_hand t in
    ignore (func hand);
    next_hand t;
    if t.currHand == initial_hand then () else map_helper ()
  in
  map_helper ()

let index_of t (hand : Hand.t) =
  match List.find_index (fun h -> h == hand) t.hands with
  | Some i -> i
  | None -> failwith "Unexpected result: hand not in group" [@coverage off]

let remove_hand t hand =
  let idx = index_of t hand in
  let new_hands = List.filteri (fun i _ -> i != idx) t.hands in
  t.hands <- new_hands;
  increase_index_if_necessary t idx
