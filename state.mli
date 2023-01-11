(**Representation of the state of a game of bridge. 

   This module represents the data stored in one game of bridge. This 
   represents one game that may be part of a match.*)

(**The abstract type of values representing bridge games. *)
type t

(**The type of player identifiers *)
type player

(**The type of card suit. *)
type suit = Club | Diamond | Heart | Spade

(**The type of card rank. *)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | 
            Jack | Queen | King | Ace

(**The type of card identifiers *)
type card = rank * suit

(**The cardinal directions which serve to identify players and their teams *)
type direction = North | East | West | South

(**Game score is a representation of the current score. It is passable to the
   Score_screen module. 
   AI: game_score.aboveline represents the aboveline scores. The first 
   component is a list of aboveline scores for the North, South team. The 
   second component is a list of aboveline scores for the East, West team.
   The belowline list is a list of belowline game scores. Each game score 
   contains first the belowline round scores of the North South team from that 
   game as well as the belowline round scores for the East West team from that
   game. game_score.team1 is the names of the players on the North South team
   and game_score.team2 is the names of the players on the East West team.*)
type game_score = 
  {aboveline : (int list) * (int list) ; belowline : (int list * int list) list; 
   team1 : string list; team2 : string list}

(**[InvalidCard] is raised when an invalid card is encountered. *)
exception InvalidCard of card

(** [dummy_of g] is the dummy of [g]. 
    Requires [g] has a dummy. *)
val dummy_of : t -> player

(** [set_dummy g p] is [g] with the dummy set to [p]. *)
val set_dummy : player -> t -> t

(** [is_dummy p g] is true iff the name of [p] matches the name of the dummy
    in [g] and false otherwise. *)
val is_dummy : player -> t -> bool

(** [update_dummy g] is [g] with the dummy updated to any changes in hand or
    score. *)
val update_dummy : t -> t

(**[players_of t] is a list of the players associated with the state [t]. *)
val players_of : t -> player list

(**[names_of players] is a list of the names of the players in the list [players]*)
val names_of : player list -> string list

(**[name_of player] is the name of the player [player]. *)
val name_of : player -> string

(**[hand_of player] is the hand of the player [player]. *)
val hand_of : player -> card list

(**[direction_of player] is the direction of the player [player]. *)
val direction_of : player -> direction

(**[rank_of card] is the rank of the card [card]. *)
val rank_of : card -> rank

(**[suit_of card] is the suit of the card [card]. *)
val suit_of : card -> suit

(** [partner_of players p] is the player in [players] with the opposite
    direction of [p]. Example: If the direction of [p] is [North] then
    [partner_of players p] is the player with direction [South]. *)
val partner_of : player list -> player -> player

(** [suit_val c] is 1 if the suit of [c] is Club, 2 if [Diamond], 3 if [Heart],
    and 4 if [Spade]. *)
val suit_val : card -> int

(** [rank_val c] is the numerical value of the rank of [c]. [Jack], [Queen],
    [King], and [Ace] are 11, 12, 13, and 14, respectively. *)
val rank_val : card -> int

(**[make_deck ()] is a full deck of cards sorted in alphabetical order by suit
   and then ascending order by rank. *)
val make_deck : unit -> card list

(**[player_from_direction lst dir] is the player that corresponds to direction
   [dir] in [lst].
   Requires: [lst] is a list of the players in a game of bridge.*) 
val player_from_direction : player list -> direction -> player

(** [next_player players p] is the player in [players] with direction that is 
    left of the direction of [p] by the following rule: [North] => [East] => 
    [South] => [West] => [North]. *)
val next_player : player list -> player -> player

(** [sort_by_direction g d] is [g] with the players of [g] sorted with the 
    player with direction [d] first, and then in the order specified by the 
    rule in [next_player]. *)
val sort_by_direction : t -> direction -> t

(** [compare_cards c1 c2] is 1 if [c1] is greater than [c2], -1 if [c1] is less
    than [c2] and 0 if they are equal. Magnitude is determined by the numerical
    value of the suit and then the rank. *)
val compare_cards : card -> card -> int

(**[sort_cards c] is [c] sorted in alphabetical order of suit and then in
   ascending order by rank, where [Two] is the lowest and [Ace] is the 
   highest. *)
val sort_cards : card list -> card list

(**[new_game p] is a game that is in bidding state with players [p].
   Each player is dealt 13 cards and players in NESW order.
   Requires: [p] has length 4. *)
val new_game : string list -> t 

(**[reshuffle_hands t] is a new game state with the score and players the same 
   but the card hands have been reshuffled. *)
val reshuffle_hands : t -> t

(**[play_card game player card] is a game of bridge with [card] removed from 
   the hand of [player] and everything else the same as [game]. 
   Raises: InvalidCard if [card] is not in the hand of [player].*)
val play_card : t -> player -> card -> t 

(**[next_player players player] is the player from the list [players]
   who will play after [player]. Requires: [players] is the list of all players
   in a game state.*)
val next_player : player list -> player -> player

(**[add_round_score state (aboveline, belowline) player] adds [aboveline] to the
   aboveline score for [player] in [state] and [belowline] to the belowline 
   score for [player] in state. *)
val add_round_score : t -> int * int -> player -> t

(**[game_score_of t] is the game_score that is associated with [t]. *)
val game_score_of : t -> game_score

(**[rubber_winner game_score] is an option which is None if there is no winner.
   If there is a winner, it is a direction of one of the players in the team 
   that won. *)
val rubber_winner : game_score -> direction option

(**[is_vulnerable state player] is true if [player] is vulnerable according to 
   the current [state]. (The rules for determining if someone is vulnerable 
   comes from wikipedia's rules for rubber bridge) *)
val is_vulnerable : t -> player -> bool

(**[final_points state player] is the final number of points that [player] would
   receive in the game [state]. If the rubber is over, this function will add
   in the rubber bonus. *)
val final_points : t -> player -> int 