type t = {
  name : string;
  mutable money : int;
  hands : HandGroup.t;
}
(** Represents a player in the game, with a name, some amount of money, and a
    mutable hand of cards. *)

val init : string -> int -> t
(** [init n m] is a new player with name [n], money [m], and an empty hand. *)

val add_card_to_hand : t -> Card.t -> unit
(** [add_card_to_hand p c] adds [c] to [p]'s hand. *)

val reset : t -> int -> unit
(** [reset p w] resets [p]'s hand to empty with wager [w]. *)

val can_special : t -> Hand.t -> bool
(** [can_special p h] is whether or not [p] can special move with [h]. *)

val can_split : t -> Hand.t -> bool
(** [can_split p h] is whether or not [p] can split with [h]. *)

val split : t -> Hand.t -> Hand.t * Hand.t
(** [split p h] splits [p]'s hand [h], and returns the two resulting hands. *)

val shadow : t -> Hand.t -> int -> Card.t * Hand.t
(** [shadow p h num] shadows [p]'s hand [h] for [num] of 1 or 2, and returns the
    shadowed card and the created hand. *)

val surrender : t -> Hand.t -> int
(** [surrender p hand] surrenders [p]'s hand [h], and is the amount of money returned to the player. *)