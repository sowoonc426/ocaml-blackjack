type t = {
  mutable cards : Card.t list;
  mutable wager : int;
}

let empty wager = { cards = []; wager }
let empty_unwagered () = empty 0
let init cards wager = { cards; wager }
let init_unwagered cards = init cards 0
let value t = Card.value_cards t.cards
let add_card t c = t.cards <- c :: t.cards

let swap_card t1 i1 t2 i2 =
  let set_idx_card t card idx =
    let new_cards =
      List.mapi (fun i c -> if i != idx then c else card) t.cards
    in
    t.cards <- new_cards
  in
  let c1 = List.nth t1.cards i1 in
  let c2 = List.nth t2.cards i2 in
  set_idx_card t1 c2 i1;
  set_idx_card t2 c1 i2

let update_to_match t1 t2 =
  t1.cards <- t2.cards;
  t1.wager <- t2.wager

let can_split t =
  match t.cards with
  | [ card1; card2 ] -> card1.value = card2.value
  | _ -> false
