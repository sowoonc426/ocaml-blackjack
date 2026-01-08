type t = {
  name : string;
  mutable money : int;
  hands : HandGroup.t;
}

let init name money = { name; money; hands = HandGroup.empty () }

let add_card_to_hand (player : t) (card : Card.t) =
  HandGroup.add_card_to_curr_hand player.hands card

let reset (player : t) wager =
  HandGroup.reset player.hands wager;
  player.money <- player.money - wager

let can_special (player : t) (hand : Hand.t) = player.money >= hand.wager

let can_split (player : t) (hand : Hand.t) =
  Hand.can_split hand && can_special player hand

let split (player : t) hand =
  if not (can_split player hand) then failwith "Hand is not allowed to split."
  else
    match hand.cards with
    | [ card1; card2 ] ->
        hand.cards <- [ card1 ];
        let idx = HandGroup.index_of player.hands hand in
        let hand2 = Hand.init [ card2 ] hand.wager in
        HandGroup.insert_hand player.hands hand2 idx;
        player.money <- player.money - hand.wager;
        (hand, hand2)
    | _ -> failwith "Unreachable code." [@coverage off]

let shadow (player : t) hand card_num =
  if not (can_special player hand) then
    failwith "Hand is not allowed to shadow."
  else
    let hand_idx = HandGroup.index_of player.hands hand in
    let card = List.nth hand.cards (card_num - 1) in
    let new_hand = Hand.init [ card ] hand.wager in
    HandGroup.insert_hand player.hands new_hand hand_idx;
    player.money <- player.money - hand.wager;
    (card, new_hand)

let surrender (player : t) (hand : Hand.t) =
  let wager = hand.wager in
  HandGroup.remove_hand player.hands hand;
  let amount_gained = wager / 2 in
  player.money <- player.money + amount_gained;
  amount_gained
