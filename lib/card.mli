type suite =
  | Spades
  | Hearts
  | Clubs
  | Diamonds
      (** The suit of a card, which can be Spades, Hearts, Clubs, or Diamonds. *)

type value =
  | Number of int
  | Ace
  | Jack
  | Queen
  | King
      (** The value of a card, which can be a Number from 2 to 10, Ace, Jack,
          Queen, or King. *)

type t = {
  suite : suite;
  value : value;
}
(** The type of a card. *)

val make : suite -> value -> t
(** [make s s] creates a card with suite [s] and value [v]. *)

val value_cards : t list -> int
(** [value t] is the numberical value of card list [t]. *)

val contains_high_ace : t list -> bool
(** [contains_high_act t] is whether the card list [t] contains an 11-value ace. *)

val all_possibilities : t list
(** [all_possibilities] is a [list] of all possible cards. *)

val all_possibilities_string : string list
(** [all_possibilities_string] is a hard-coded [list] of all stringified cards. *)

val string : t -> string
(** [string t] is the card [t] as a string. *)
