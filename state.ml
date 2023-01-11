(* module that represents a game and its state *)

type suit = Club | Diamond | Heart | Spade
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | 
            Jack | Queen | King | Ace
type card = rank * suit
type direction = North | East | West | South
type player = {name : string; hand : card list; 
               direction : direction}
type game_score = 
  {aboveline : (int list) * (int list); 
   belowline : (int list * int list) list; 
   team1 : string list; team2 : string list}

type t = {dummy : player option; players : player list; game_score : game_score}
exception InvalidCard of card
exception InvalidPlayer of player
exception InvalidRankValue of int

let dummy_of game =
  Option.get game.dummy

let set_dummy d game =
  {game with dummy = Some d}

let is_dummy d game =
  try (dummy_of game).name = d.name
  with Invalid_argument _ -> false

let update_dummy game =
  let dummy = dummy_of game in
  let dummy' = List.find (fun x -> x.name = dummy.name) game.players in
  {game with dummy = Some dummy'}

let players_of game =
  game.players

let names_of players =
  List.fold_right (fun x acc -> x.name::acc) players []

let name_of player =
  player.name

let hand_of player =
  player.hand

let direction_of player =
  player.direction

let rank_of (card : card) =
  fst card

let suit_of (card : card) =
  snd card

let partner_direction_of = function
  | North -> South
  | South -> North
  | West -> East
  | East -> West

let partner_of players player =
  let partner_direction = player.direction |> partner_direction_of in
  List.find (fun p -> p.direction = partner_direction) players

let val_of_suit = function
  | Club -> 1
  | Diamond -> 2
  | Heart -> 3
  | Spade -> 4

let val_of_rank = function
  | Two -> 2   | Three -> 3  | Four -> 4 
  | Five -> 5  | Six -> 6    | Seven -> 7
  | Eight -> 8 | Nine -> 9   | Ten -> 10 
  | Jack -> 11 | Queen -> 12 | King -> 13
  | Ace -> 14

(**[rank_of_val n] is the corresponding rank of [n]. Raises [InvalidRankValue] 
   if [n] is not in 2..14.
   Examples: [rank_of_val 2] is [Two]
   [rank_of_val 13] is [King]
   [rank_of_val 14] is [Ace] *)
let rank_of_val = function
  | 2 -> Two   | 3 -> Three  | 4 -> Four
  | 5 -> Five  | 6 -> Six    | 7 -> Seven
  | 8 -> Eight | 9 -> Nine   | 10 -> Ten
  | 11 -> Jack | 12 -> Queen | 13 -> King
  | 14 -> Ace  | invalid_num -> raise (InvalidRankValue invalid_num)

let suit_val card =
  card |> suit_of |> val_of_suit

let rank_val card =
  card |> rank_of |> val_of_rank

let compare_cards card1 card2 =
  let suit_val_1 = suit_val card1 in
  let suit_val_2 = suit_val card2 in
  let rank_val_1 = rank_val card1 in
  let rank_val_2 = rank_val card2 in
  let compare_val = Stdlib.compare suit_val_1 suit_val_2 in
  if compare_val = 0 then Stdlib.compare rank_val_1 rank_val_2 else compare_val

let sort_cards =
  List.sort compare_cards

(**[cards_of_suit s c] is all the cards of [s] in ascending order prepended 
   to [c]*)
let cards_of_suit (suit : suit) (old_card_lst : card list) =
  let rec full_suit_helper acc num = 
    if num = 1 then acc
    else full_suit_helper ((rank_of_val num, suit)::acc) (num - 1)
  in full_suit_helper old_card_lst 14

(**[make_deck u] is a length-52 [card list] sorted in alphabetical order of
   suit followed by ascending order of rank. [make_deck] is tail-recursive.*)
let make_deck () =
  let rec make_deck_helper acc = function
    | Club -> cards_of_suit Club acc
    | Diamond -> make_deck_helper (cards_of_suit Diamond acc) Club
    | Heart -> make_deck_helper (cards_of_suit Heart acc) Diamond
    | Spade -> make_deck_helper (cards_of_suit Spade acc) Heart
  in make_deck_helper [] Spade

(**[distribute_cards p c] is [p] with each 12-element sublist distributed to 
   the respective element of [p]. [distribute_cards] is tail-recursive.
   Requires: The length of [p] is 4 and each element has an empty hand.
   Requires: The length of [c] is 52.*)
let distribute_cards player_lst card_lst =
  let rec distribute_helper acc deck = function
    | [] -> acc
    | h::t -> if List.length h.hand < 13 then 
        begin
          match deck with
          | [] -> failwith "Invalid number of cards in deck"
          | x::xs -> distribute_helper acc xs ({h with hand = x::h.hand}::t)
        end
      else let sorted_player = {h with hand = sort_cards h.hand} in
        distribute_helper (sorted_player::acc) deck t
  in distribute_helper [] card_lst player_lst

let next_direction = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let player_from_direction lst dir = 
  List.find_opt (fun x -> direction_of x = dir) lst |> Option.get

let next_player players current_player =
  direction_of current_player |> next_direction |> player_from_direction players

let sort_by_direction game dir =
  let rec sbd_helper players =
    match players with
    | [] -> failwith "impossible"
    | h::t -> if h.direction = dir then players 
      else sbd_helper (t @ [h])
  in
  let new_players = sbd_helper game.players in
  {game with players = new_players}

(**[initialize_players n] is a [player list] where the name of each player
   is the respective element of [n]. All players start with a score of 0 and
   an empty hand. *)
let initialize_players name_lst =
  List.fold_left (fun (acc, direction) h -> 
      let new_direction = next_direction direction in
      let new_player = {name = h; hand = []; direction = direction} 
      in (new_player::acc), new_direction) ([], North) name_lst |> fst

let deal_cards player_lst =
  ()
  |> make_deck 
  |> Shuffle.shuffle
  |> distribute_cards player_lst 

let new_game name_lst = 
  let player_lst = initialize_players name_lst in 
  let players = deal_cards player_lst
  in let team1names = [name_of (player_from_direction players North); 
                       name_of (player_from_direction players South)] 
  in let team2names = [name_of (player_from_direction players East);
                       name_of (player_from_direction players West)] 
  in 
  {dummy = None; players = players; 
   game_score = {aboveline = [],[]; belowline = []; 
                 team1 = team1names; team2 = team2names}}

let reshuffle_hands game =
  let players' = List.map (fun x  -> {x with hand = []}) game.players 
                 |> deal_cards 
  in {game with players = players'}

(**[remove_card c h] is [h] with [c] removed. Raises [InvalidCard]
   if [c] is not an element of [h]. *)
let remove_card card hand =
  if List.mem card hand then List.filter (fun x -> x != card) hand
  else raise (InvalidCard card)

let play_card game player card =
  let new_hand = if List.mem player game.players 
    then remove_card card player.hand
    else raise (InvalidPlayer player) in
  let change_hand x = 
    if x.name = player.name then {x with hand = new_hand} else x 
  in {game with players = List.map change_hand game.players}

let rec sum_of list sum = 
  match list with 
  | h :: t -> sum_of t (sum + h)
  | [] -> sum

let add_belowline_scores list score team1= 
  match list with 
  | (team1score, team2score) :: t -> if score > 0 then 
      (if sum_of team1score 0 >= 100 || sum_of team2score 0 >= 100 
       then (if team1 then ([score],[]) :: list else ([], [score]) :: list)
       else (if team1 then (score :: team1score, team2score) :: t 
             else (team1score, score :: team2score) :: t)) else list
  | [] -> if team1 then [[score], []] else [[], [score]]

let add_round_score state (above, below) player =
  let old_score = state.game_score in 
  let new_score = 
    if direction_of player = North || direction_of player = South then
      let new_above_line =
        match old_score.aboveline with 
          (team1score, team2score ) -> if above > 0 
          then (above :: team1score, team2score) 
          else if above < 0 then (team1score, (-above) :: team2score) 
          else (team1score, team2score)
      in {aboveline = new_above_line ; 
          belowline = add_belowline_scores old_score.belowline below true;
          team1 = old_score.team1; team2 = old_score.team2}
    else 
      let new_above_line =
        match old_score.aboveline with 
          (team1score, team2score ) -> if above > 0 
          then (team1score,above :: team2score) 
          else if above < 0 then ((-above) :: team1score, team2score)
          else (team1score, team2score)
      in {aboveline = new_above_line; 
          belowline = add_belowline_scores old_score.belowline below false; 
          team1 = old_score.team1; team2 = old_score.team2}
  in {dummy = state.dummy; players= state.players; game_score = new_score}

let game_score_of state = 
  state.game_score

let rec end_rubber_helper belowline (team1acc, team2acc) = 
  match belowline with 
  | (below1, below2) :: t -> 
    let team1 = sum_of below1 0 
    in let team2 = sum_of below2 0
    in if team1 >= 100 
    then end_rubber_helper t (team1acc + 1, team2acc) 
    else if team2 >= 100 
    then end_rubber_helper t (team1acc, team2acc + 1) 
    else end_rubber_helper t (team1acc, team2acc)
  | [] -> if team1acc >= 2 then Some North 
    else if team2acc >= 2 
    then Some East 
    else None

let rubber_winner score  = 
  end_rubber_helper score.belowline (0,0)

let rec is_vulnerable_helper belowline player = 
  if direction_of player = North || direction_of player = South then 
    match belowline with 
    | (x, y) :: t -> if sum_of x 0 >= 100 then true 
      else is_vulnerable_helper t player 
    | _ -> false
  else match belowline with 
    | (x, y ) :: t -> if sum_of y 0 >= 100 then true 
      else is_vulnerable_helper t player 
    | _ -> false

let is_vulnerable state player = 
  match state.game_score.belowline with 
  | l  :: tail -> is_vulnerable_helper tail player
  | _ -> false

let rec sum_belowline belowline sum team1 =
  match belowline with 
  | (t1, t2) :: t -> if team1 then sum_belowline t ((sum_of t1 0) + sum) true 
    else sum_belowline t ((sum_of t2 0) + sum) false 
  | [] -> sum

let is_winner player gamescore =
  let win_direction = rubber_winner gamescore in 
  let direction = direction_of player in 
  match win_direction with 
  | None -> false
  | Some (East) | Some (West) -> if direction = East || direction = West 
    then true else false
  | Some (North) | Some (South) -> if direction = North || direction = South 
    then true else false

let rec other_team_won_one belowline player =
  match belowline with 
  | (t1, t2) :: t -> 
    if direction_of player = North || direction_of player = South then 
      if sum_of t2 0 >= 100 then true else (other_team_won_one t player)
    else if sum_of t1 0>= 100 then true else (other_team_won_one t player)
  | [] -> false

let final_points state player =
  let gamescore = state.game_score in 
  let direction = direction_of player in
  let above_points = 
    match gamescore.aboveline with 
    | (team1, team2) -> 
      if direction = North || direction = South 
      then sum_of team1 0 else sum_of team2 0 in
  let below_points = 
    sum_belowline gamescore.belowline 0 (direction = North || direction = South)
  in let before_bonus = above_points + below_points in 
  if is_winner player gamescore
  then if other_team_won_one gamescore.belowline player 
    then before_bonus + 500 else before_bonus + 700 
  else before_bonus


