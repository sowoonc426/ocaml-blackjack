type t = {
  mutable hands : Hand.t list;
  mutable currHand : int;
}
(** Represents a player's group of hands, and the current hand being actively used by the player. *)

val empty : unit -> t
(** [empty ()] is a newly initialized empty HandGroup *)

val is_empty : t -> bool
(** [is_empty g] is whether [g] is empty. *)

val index_of : t -> Hand.t -> int
(** [index_of g h] is the index of hand [h] in [g]. *)

val curr_hand : t -> Hand.t
(** [curr_hand g] is the current hand of [g]. *)
val add_card_to_curr_hand : t -> Card.t -> unit
(** [add_card_to_curr_hand g c] adds card [c] to the current hand of [g]. *)

val next_hand : t -> unit
(** [next_hand g] rotates [g] to the next hand. *)
val insert_hand : t -> Hand.t -> int -> unit
(** [insert_hand g h i] inserts [h] into [g] at index [i]. *)
val remove_hand : t -> Hand.t -> unit
(** [remove_hand g h i] removes [h] from [g]. *)
val reset : t -> int -> unit
(** [reset_hand g w] resets [g] to default, which is a single empty hand with wager [w].  *)

val map_over_hands : t -> (Hand.t -> 'a) -> unit
(** [map_over_hands g f] maps function f over all hands in [g] in order, from the current hand to the end. *)
