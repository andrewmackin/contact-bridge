(* module for playing tricks *)
open State

(** AF: trick represents a set of 4 cards, and the suit of the first card added
    RI: No player appears more than once, the set has strictly 1 to 4 elements*)
type trick = { 
  starting_suit : State.suit;
  assoc_cards : (State.player * State.card) list;
}

(** AF: t represents a set of tricks, and the trump suit of the round
    RI: the set has strictly 1 to 13 elements. The same 4 players appear in each
    trick. Each card appears in exactly 1 trick.*)
type t = {
  trump_suit : State.suit option;
  tricks : trick list;
}

exception InvalidCardPlayed of State.card
exception InvalidTrick of trick * string

let initalize_tricks trump = 
  {trump_suit = trump; tricks = []}

let trump_suit t = 
  t.trump_suit

let past_tricks t =
  t.tricks

let update_tricks t tr = 
  {trump_suit = trump_suit t; tricks = tr::past_tricks t}

let starting_suit tr =
  tr.starting_suit

let player_cards tr =
  tr.assoc_cards

(** [is_card_in_hand c p] is true iff c is in the hand of p else false*)
let is_card_in_hand c p =
  List.mem c (State.hand_of p) 

(** [make_trick first_card first_player] is a trick initalized with the player
    card pair and with the [starting_suit] initalized as the suit of the card*)
let make_trick (first_card : State.card) (first_player : State.player) = 
  if not (is_card_in_hand first_card first_player) 
  then raise (InvalidCardPlayed first_card)
  else {
    starting_suit = State.suit_of first_card; 
    assoc_cards = [first_player, first_card];
  }

let make_empty_trick (unit : unit) = 
  {starting_suit = State.Heart; assoc_cards=[]}

let add_card_helper card tr =   
  if List.length tr.assoc_cards = 0 then 
    {starting_suit=State.suit_of card;assoc_cards=[]}
  else tr

(** [add_card card player tr] must check to ensure several properties of its
    input that the card is in the player hand, that if it is its the same suit 
    as the starting suit and if its not the hand has no cards of the starting 
    suit, in order to ensure the function raises the appropriate exception*)
let add_card card player tr = 
  let tr = add_card_helper card tr in
  if not (is_card_in_hand card player) || 
     (State.suit_of card <> starting_suit tr &&
      List.exists (fun x -> State.suit_of x = starting_suit tr) 
        (State.hand_of player))
  then raise (InvalidCardPlayed card)
  else if List.length (player_cards tr) = 4 then 
    raise (InvalidTrick (tr, "Could not add card, trick already has 4 cards")) 
  else if List.mem player (match List.split (player_cards tr) with | (h,t) -> h)
  then raise (InvalidTrick (tr, "Player has already played a card"))
  else {
    starting_suit = starting_suit tr;
    assoc_cards = (player,card)::player_cards tr
  }

let is_trick_done tr = 
  List.length tr.assoc_cards = 4

(** [suit_cull pc s] is pc with cards not of the same suit as s removed*)
let rec suit_cull pc s = 
  match pc with
  | (a,b)::t -> if State.suit_of b = s 
    then (a,b)::(suit_cull t s) 
    else suit_cull t s
  | [] -> []

(** [suit_cull_main t tr] is the player card list of trick with only cards of
    one suit. If atleast 1 card had the trump suit that list contains only cards
    with the trump suit, otherwise it contains only cards with the starting suit
*)
let suit_cull_main t tr =
  if List.length (player_cards tr) <> 4 then raise 
      (InvalidTrick (tr, "Trick did not have 4 cards")) else
    let trump = trump_suit t in
    match trump with 
    | Some c -> let sc = suit_cull (player_cards tr) c in begin
        match sc with
        | [] -> suit_cull (player_cards tr) (starting_suit tr)
        | _ -> sc
      end
    | None -> suit_cull (player_cards tr) (starting_suit tr)

(** [highest_rank l p r] is the player whos card had the highest rank of list
    l. r is that value and p is that player.*)
let rec highest_rank l p r =
  match l with
  | (a,b)::t -> if State.rank_val b > r 
    then highest_rank t a (State.rank_val b) 
    else highest_rank t p r
  | [] -> p

(** [winner t tr] computes the winner using the three previous helper functions
    it computes the player and the rank of their card for the first player of 
    the list and uses that to call the helper functions*)
let winner t tr = 
  match player_cards tr with
  | (a,b)::tail -> highest_rank (suit_cull_main t tr) a (State.rank_val b)
  | [] -> raise (InvalidTrick (tr, "Could not determine winner")) 
