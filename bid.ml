(*module for bidding*)

(**The type that represents a bid. *)

type bid =
  |Pass 
  |Double 
  |Redouble 
  |Clubs of int 
  |Diamonds of int 
  |Hearts of int 
  |Spades of int 
  |NoTrump of int


(**The type that represents a bid_state. [bid_list] is an association list of 
   the players and their bids. If player1 made the bid Clubs 2 it would be 
   represented as [(player1, Clubs 2)]. The most recent bid is first. [game] is
   the game that is associated with this set of bids. *)
type t = {
  bid_list : (State.player * bid) list; game : State.t
}

type teams = {team1 : State.player list ; team2 : State.player list}

exception InvalidBid of bid

exception InvalidPlayer of State.player

let game_of t =
  t.game

let new_bidding game =
  {bid_list = []; game = game}

let last_bid (bid_state : t) = 
  let bid_list = bid_state.bid_list in
  let rec get_bid list_of_bids =
    match list_of_bids with 
    | [] -> None
    | (player, bid) :: t -> 
      if bid = Pass || bid = Double || bid = Redouble 
      then get_bid t else Some (player, bid)
  in get_bid bid_list 

let trump_suit_of t =
  match last_bid t with
  | Some (_, bid) -> begin
      match bid with
      | Pass | Double | Redouble -> failwith "Invalid trump suit"
      | Clubs _ -> Some State.Club
      | Diamonds _-> Some State.Diamond
      | Hearts _ -> Some State.Heart
      | Spades _ -> Some State.Spade
      | NoTrump _ -> None
    end
  | None -> failwith "Does not have a trump suit"

(**[bigger bid1 bid2] is true if [bid1] is strictly bigger than [bid2] according
   to the rules of bridge bidding. The order of suits is Clubs, Diamonds,
   Hearts, Spades, NoTrump. Thus, a bid of Clubs 1 is smaller than a bid of 
   Diamonds 1. However, a bid of Clubs 2 is bigger than a bid of Diamonds 1. *)
let bigger bid1 bid2 = 
  match bid1 with 
  | Clubs x -> begin 
      match bid2 with
      | Clubs y | Diamonds y | Hearts y | Spades y | NoTrump y -> 
        if x > y then true else false
      | _ -> false
    end
  | Diamonds x ->  begin 
      match bid2 with
      | Clubs y -> if x >= y then true else false
      | Diamonds y | Hearts y | Spades y | NoTrump y -> 
        if x > y then true else false
      | _ -> false
    end
  | Hearts x ->  begin 
      match bid2 with
      | Clubs y | Diamonds y -> if x >= y then true else false
      | Hearts y | Spades y | NoTrump y -> if x > y then true else false
      | _ -> false
    end
  | Spades x ->  begin 
      match bid2 with
      | Clubs y | Diamonds y | Hearts y -> if x >= y then true else false
      | Spades y | NoTrump y -> if x > y then true else false
      | _ -> false
    end
  | NoTrump x ->  begin 
      match bid2 with
      | Clubs y | Diamonds y | Hearts y | Spades y -> 
        if x >= y then true else false
      | NoTrump y -> if x > y then true else false
      | _ -> false
    end
  | _ -> false

(**[on_team bid_state player1 player2] is true if player1 and player2 are on 
   the same team in bid_state. *)
let on_team bid_state player1 player2  = 
  let player_list = State.players_of bid_state.game in 
  let rec get_index acc list player = 
    match list with 
    | [] -> acc 
    | h :: t -> if h = player then acc else get_index (acc + 1) t player
  in if (get_index 0 player_list player1) mod 2 = 
        (get_index 0 player_list player2) mod 2 
  then true else false

(**[other_team_bet player1 bid_state] is true if the other team has bet a
   bid that has both a number and suit in the bid_state [bid_state] *)
let other_team_bet player1 bid_state= 
  let bid_list = bid_state.bid_list in 
  let rec last_bet_other_team bids = 
    match bids with 
    | [] -> false
    | (player, bid) :: t -> if bid = Pass || bid = Double || bid = Redouble || 
                               (on_team bid_state player player1) = true 
      then last_bet_other_team t else true
  in last_bet_other_team bid_list

let other_team_double bid_state player1 = 
  let rec find_double bids = 
    match bids with 
    | [] -> false
    | (player2, last_bid) :: t -> if last_bid = Double 
                                  && on_team bid_state player1 player2 = false 
      then true else find_double t
  in find_double bid_state.bid_list

let add_suit_bid bid_state bid player x= 
  if 
    (match (last_bid bid_state) with 
     | None -> true
     | Some (player2, bid2) -> bigger bid bid2 )
    && x > 0 && x < 8
  then 
    {bid_list = (player, bid) :: bid_state.bid_list; 
     game = bid_state.game} 
  else raise (InvalidBid bid)

(**[bid bid_state player bid] adds (player, bid) to a bid_state if [bid] is 
   a legal [bid] *) 
let bid bid_state player bid = 
  if List.mem player (State.players_of bid_state.game) = false 
  then raise (InvalidPlayer player) 
  else match bid with 
    | Pass -> 
      {bid_list = (player, bid) :: bid_state.bid_list; game = bid_state.game}
    | Double -> if other_team_bet player bid_state 
      then {bid_list = (player, bid) :: bid_state.bid_list; 
            game = bid_state.game} 
      else raise (InvalidBid bid)
    | Redouble -> if other_team_double bid_state player then
        {bid_list = (player, bid) :: bid_state.bid_list; game = bid_state.game} 
      else raise (InvalidBid bid)
    | Clubs x | Diamonds x | Spades x | Hearts x | NoTrump x -> 
      add_suit_bid bid_state bid player x

let previous_bids bid_state = 
  bid_state.bid_list

let three_passes bid_list = 
  match bid_list with 
  | (p1 , b1) :: (p2 , b2) :: (p3, b3) ::  t -> 
    if b1 = Pass && b2 = Pass && b3 = Pass 
    then true else false 
  | _ -> false

let same_team player1 player2 = 
  let direction2 = State.direction_of player2 in 
  match State.direction_of player1 with 
  | State.North | State.South -> direction2 = South || direction2 = North 
  | State.East | State.West -> direction2 = West || direction2 = East

let same_suit bid1 bid2 = 
  match bid1 with 
  | Clubs _ -> begin match bid2 with 
      | Clubs _ -> true 
      | _ -> false
    end
  | Diamonds _ -> begin match bid2 with 
      | Diamonds _ -> true 
      | _ -> false
    end
  | Hearts _ -> begin match bid2 with 
      | Hearts _ -> true 
      | _ -> false
    end
  | Spades _ -> begin match bid2 with 
      | Spades _ -> true 
      | _ -> false
    end
  | NoTrump _ -> begin match bid2 with 
      | NoTrump _ -> true 
      | _ -> false
    end
  | _ -> failwith "Invalidates precondition"

let rec declarer_helper player1  bid bids = 
  match bids with 
  | (player2, bid2) :: t -> 
    if same_team player1 player2 then 
      (if same_suit bid bid2 then declarer_helper player2 bid t else player1) 
    else declarer_helper player1 bid t
  | [] -> player1

let declarer_of bids = 
  let final_bid = last_bid bids in 
  match final_bid with 
  | Some (player1, bid) -> declarer_helper player1 bid (previous_bids bids)
  | _ -> failwith "Invalidates precondition"