type t = Card.t list ref

exception Empty

let init () = ref Card.all_possibilities

let init_broken () =
  (ref (List.init 52 (fun _ -> Card.(make Spades Ace))) [@coverage off])

let from_cards cards = ref cards
let is_empty t = List.is_empty !t
let reset t = t := Card.all_possibilities

let next_card t =
  match !t with
  | [] -> raise Empty
  | h :: tl ->
      (* Remove the first card from t. *)
      t := tl;
      h

let shuffle_deck (deck : t) : t =
  let deck_array = Array.of_list !deck in
  let len = Array.length deck_array in
  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = deck_array.(i) in
    deck_array.(i) <- deck_array.(j);
    deck_array.(j) <- temp
  done;
  ref (Array.to_list deck_array)

let shuffle (deck : t) = deck := !(shuffle_deck deck)
