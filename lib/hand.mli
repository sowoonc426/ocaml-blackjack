type t = {
  mutable cards : Card.t list;
  mutable wager : int;
}
(** Represents a hand of cards, with a list of the cards in the hand and an amount of money wagered on the hand. *)

val empty : int -> t
(** [empty w] is a new hand with wager [w]. *)
val empty_unwagered : unit -> t
(** [empty_unwagered ()] is a new hand with wager [0]. *)
val init : Card.t list -> int -> t
(** [init cs w] is a new hand with cards [cs] and wager [w]. *)
val init_unwagered : Card.t list -> t
(** [init_unwagered cs] is a new hand with cards [cs] and wager [0]. *)
val value : t -> int
(** [value h] is the value of hand [h]. *)

val add_card : t -> Card.t -> unit
(** [add_card h c] adds card [c] to [h]. *)

val swap_card : t -> int -> t -> int -> unit
(** [swap_card h1 i1 h2 i2] swaps card index [i1] in [h1] with card index [i2] in [h2]. *)

val update_to_match : t -> t -> unit
(** [update_to_match t1 t2] updates the values of [t1] to match [t2]. *)

val can_split : t -> bool
(** [can_split h] is whether or not hand [h] is allowed to split. *)
