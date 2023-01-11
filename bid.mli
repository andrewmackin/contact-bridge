(**Bid provides utilities for managing bids and the trump suit. *)

(**Representation of bidding in a game of bridge.

   Bidding represents all of the bids that have occurred in a game.*)

(**Abstract type that represents the bids that have occurred in a game of bridge 
*)
type t

(**Represents a bid. 
   AF: Bids either have two components, a suit (or no trump) and 
   a number between 1 and 7, or they are doubling or redoubling the previous
   bid, or they are a pass. Bids are associated with a bid.*)
type bid =   
  |Pass 
  |Double 
  |Redouble 
  |Clubs of int 
  |Diamonds of int 
  |Hearts of int 
  |Spades of int 
  |NoTrump of int


(**Raised when a bid is against the rules of bridge. *)
exception InvalidBid of bid

(**Raised when a player is not part of the game that the bid_state is associated
   with.*)
exception InvalidPlayer of State.player

(**[game_of bid] is the game associated with the bid state [bid].*)
val game_of : t -> State.t

(**[new_bidding unit] is a new structure t that represents the bidding in a 
   game of bridge [unit]. *)
val new_bidding : State.t -> t 

(**[last_bid t] is the most recent bid that has a suit if there is one.*)
val last_bid : t -> (State.player * bid) option

(**[trump_suit_of t] is the trump suit of [t]. It is the suit of the last bid
   that was played which has a suit.*)
val trump_suit_of : t -> State.suit option

(**[bid bid_state game player bid] is the bidding state that is the same as 
   [bidding_state] with a [bid] made by [player] added to it. 

   Raises: InvalidBid if the [bid] is against the rules. A bid is against the 
   rules if it is a suit and integer and smaller than the most recently 
   played bid that also is a suit (or NoTrump) and integer. A bid with a higher
   number of tricks is larger than a bid with a lower number of tricks. If the 
   bids have the same number of tricks, the one with the higher suit is higher. 
   The numbers associated with a bid must be at least 1 and no more than 7.
   A bid is also against the rules if the player bids Double and there isn't a
   bid from someone on the other team that is a suit (or NoTrump) and an 
   integer. A bid is also against the rules if the player bids Redouble and 
   there isn't a Double bid from someone on the other team. 
   Raises: InvalidPlayer if the [player] is not a player in the game associated
   with the bid_state.*)
val bid : t -> State.player -> bid -> t 

(**[previous_bids bid_state] is the list of bids that have already been placed 
   in the reverse order they were placed. (The first entry in the list 
   corresponds to the most recently placed bid.*)
val previous_bids : t -> (State.player * bid) list

(**[three_passes bid_list] is true if the last three bids in [bid_list] are 
   passes. *)
val three_passes : (State.player * bid) list -> bool

(**[declarer_of bids] is the State.player which corresponds to the declarer. *)
val declarer_of: t -> State.player