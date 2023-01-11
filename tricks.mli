(** Representation of a trick played in a game of contract bridge.
    [t] represents the tricks that have been played so far in a round of 
    contract bridge. After a trump suit or NT has been decided a [t] should
    be created with [initalize_tricks trump].
    For each of the 13 rounds one of [make_trick] or [make_trick_empty] should
    be used to initalize a new [trick] players should then all add cards with
    [add_card] (Excluding the first player if [make_trick] is used). After the
    trick is complete ie the trick has four cards. The trick should be added
    to [t].*)

(** [trick] is a type that repersents the players and the cards they played
    for a specific trick*)
type trick 

(** [t] is the abstract type of tricks that includes the past tricks and the
    trump suit *)
type t

(** [InvalidTrick] is raised if a action would create an invalid trick or the 
    action could not occur with the trick given. For example determining the 
    winner of a incomplete trick*)
exception InvalidTrick of trick * string

(** [InvalidCardplayed] is raised if a card is played that was not contained in 
    the players hand was played*)
exception InvalidCardPlayed of State.card

(** [initalize_tricks trump] creates an abstract type of tricks with the trump 
    suit or None if the trump suit is NT*)
val initalize_tricks : State.suit option -> t

(** [update_tricks t tr] is t with the addition of trick [tr]
    Precondition: [tr] is a complete trick with 4 player card pairs*)
val update_tricks : t -> trick -> t

(** [trump_suit t] is the trump suit as was determined in bidding or is [None] 
    if there is no trump suit*)
val trump_suit : t -> State.suit option

(** [past_tricks t] is a list of all previous completed tricks*)
val past_tricks : t -> trick list

(** [starting_suit tr] is the suit of the first card played in a trick*)
val starting_suit : trick -> State.suit

(** [player_cards tr] is an associated list of players with the card they 
    played in the trick*)
val player_cards : trick -> (State.player * State.card) list

(** [make_trick c p] is a new trick with the starting suit set and only
    the one player card pair given as input
    Raises: InvalidCardPlayed if the card was not in the players hand*)
val make_trick : State.card -> State.player -> trick

(** [make_empty_trick] is a trick with no cards. The starting suit will be
    set when the first card is added with [add_card]. 
    This serves as an alternative function to [make_trick]. This function can be
    used before any players turn and then each player can just use [add_card]. 
    In comparison [make_trick] requires the first player to use that and the 
    following three to use [add_card].*)
val make_empty_trick : unit -> trick

(** [add_card c p tr] is [tr] with the additional player card pair added
    Raises: InvalidCardPlayed if the card was not in the players hand or the 
    player had a card of the leading suit but played a card of a different suit
    Raises: InvalidTrick if the trick already had 4 player card pairs
    or if a player who already played a card is trying to add another*)
val add_card : State.card -> State.player -> trick -> trick

(** [is_trick_done tr] is true iff [tr] has 4 player card pairs.*)
val is_trick_done : trick -> bool

(** [winner trick] returns the winner of a trick. The winner of a trick is the 
    player who played the highest card in the trump suit or the highest card 
    of the leading suit, if no cards of the trump suit where played. Players 
    must follow the suit in play unless they don't have any more cards of that 
    suit, in which case they can start playing cards of the other suits.
    Raises: InvalidTrick if there are not 4 card player pairs*)
val winner : t -> trick -> State.player