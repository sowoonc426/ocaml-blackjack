type t
(** The type of a deck. *)

exception Empty
(** Exception thrown when an empty deck is encountered in operations that cannot
    work on empty decks. *)

val init : unit -> t
(** [init] is a freshly initialized deck. *)

val init_broken : unit -> t
(** [init_broken] is a freshly initialized 'broken' deck, meaning it has 52 of
    the same card (ace of spades). *)

val from_cards : Card.t list -> t
(** [from_cards lst] is a new deck that contains cards [lst]. *)

val is_empty : t -> bool
(** [is_empty t] is whether the deck [t] is empty. *)

val reset : t -> unit
(** [reset t] will reset deck [t] to a freshly initialized deck. *)

val next_card : t -> Card.t
(** [next_card t] is the next [Card] in deck [t]. *)

val shuffle_deck : t -> t
(** [shuffle_deck t] returns a new deck with the same cards as [t] but in
    randomized order. *)

val shuffle : t -> unit
(** [shuffle t] shuffles the deck [t]. *)
