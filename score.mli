(**Score provides utilities for determining the score of a round of bridge *)
open Bid
open Tricks

(** [points b t v] represents the number of points earned by the declarer. The
    first value is the number above the line and the second below the line based
    on the bid [b]. The made tricks [t] played and if the declarer was 
    vulnerable [v] v is true iff the declarer is vulnerable [points] is 
    positive if the declarer earned points, if it is negative then the 
    defenders earned the absolute value of [fst points b t v].
    Precondition: [b] is a valid bid. [t] contains 13 tricks*)
val points : Bid.t -> Tricks.t -> bool -> int * int

(** The following functions have been exposed for testing because [points] 
    cannot be reasonably tested automatically. They should not be used.*)

val get_last_bid : ('a * Bid.bid) list -> ('a * Bid.bid)

val get_partner : State.player -> State.player list -> State.player

val get_modifier : ('a * Bid.bid) ->  ('a * Bid.bid) -> int

val decl_points : int -> int -> int -> int -> bool -> int * int

val penalty_points : int -> int -> int -> bool -> int