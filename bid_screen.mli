(**Bid screen is the screen seen during the bidding process *)

(**[bid_screen bid_state cards player] draws a bidding screen and processes 
   clicks. It represents the bid_state [bid_state] while it is the turn of 
   [player]. [player] has the hand [cards]. [bid_screen] evaluates to bid that 
   the player chose to be called *)
val bid_screen : Bid.t -> State.card list -> State.player 
  -> Bid.bid
