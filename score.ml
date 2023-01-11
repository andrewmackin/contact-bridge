(* module for scorekeeping *)
open Bid
open Tricks

(*[tricks_won] t p1 p2 is the number of tricks won by p1 and p2 in t *)
let tricks_won t p1 p2 = 
  let rec tricks_won_helper t l p1 p2 =
    match l with
    | [] -> 0
    | h::b -> let winner = Tricks.winner t h in if winner = p1 || winner = p2 
      then 1 + tricks_won_helper t b p1 p2
      else tricks_won_helper t b p1 p2 in
  tricks_won_helper t (Tricks.past_tricks t) p1 p2

let get_bid b =
  match b with (a,b) -> b

(*[get_last_bid l] is the last non-pass bid in l*)
let rec get_last_bid l =
  match l with
  | h::t -> if get_bid h <> Bid.Pass then h else get_last_bid t
  | [] -> failwith "All bids where Pass"

(**[get_modifier l e] returns 1 if e = l. 2 if e = Bid.Double and 4 otherwise*)
let get_modifier l eff =
  if get_bid eff = get_bid l then 1 else 
  if get_bid eff = Bid.Double then 2 else 
  if get_bid eff = Bid.Redouble then 4 else 
    failwith "Failed to get Bid Modifier"

(* Values are 2 * what they should be so that they are all ints*)
(** Gets a modifier associated with minor v major v NT suites.*)
let get_suit b =
  match b with 
  | Pass       -> failwith "Bid was Pass"
  | Double     -> failwith "Bid was Double"
  | Redouble   -> failwith "Bid was Redouble"
  | Clubs    x -> 2
  | Diamonds x -> 2
  | Hearts   x -> 3
  | Spades   x -> 3
  | NoTrump  x -> 4

let get_num_of_bid b =
  match b with 
  | Pass       -> failwith "Bid was Pass"
  | Double     -> failwith "Bid was Double"
  | Redouble   -> failwith "Bid was Redouble"
  | Clubs    x -> x
  | Diamonds x -> x
  | Hearts   x -> x
  | Spades   x -> x
  | NoTrump  x -> x

(**[deck_points b w m s v] is the number of points earned by the declarer. Based
   on the number that was bid, the number of tricks won, if the bid was doubled
   or redoubled, the suit of the bid and if the declarer was vulnerable. *)
let decl_points b_num w_num modifier suit vul =
  let over_trick = (
    (if modifier = 2 then 100 else if modifier = 4 then 200 else 0) * 
    (if vul then 2 else 1) +
    (if modifier <> 1 then 0 else if suit = 2 then 20 else 30)) * 
    (w_num - b_num) in
  let slam = (if b_num = 12 then 500 else if b_num = 13 then 1000 else 0) in
  let double_re = (if modifier = 1 then 0 else 25 * modifier) in
  let contract_points = (
    if suit <> 4 
    then ((b_num * 20 * modifier * suit) / 2)
    else (40 + 30 * (b_num - 1))) in
  (over_trick + slam + double_re, contract_points)

(**[penalty_points b w m v] is the number of points earned by the defender. 
   Based on the number that was bid, the number of tricks won, if the bid was
   doubled or redoubled, and if the declarer was vulnerable.*)
let penalty_points b_num w_num modifier vul = 
  if modifier = 1 then (b_num - w_num) * 50 * (if vul then 2 else 1) * -1 else 
    (if vul then 
       (200 + 300 * (b_num - w_num - 1)) else 
       (100 + 200 * (if b_num - w_num - 1 < 3 then b_num - w_num - 1 else 2) + 
        300 * (if b_num - w_num - 3 > 0 then b_num - w_num - 3 else 0))
    ) 
    * (if modifier = 2 then 1 else 2) * -1

(**[get_partner p l] returns the partner of p.*)
let rec get_partner p l = 
  let direction_comp a b = 
    match a with
    | State.North -> b = State.South
    | State.South -> b = State.North
    | State.East  -> b = State.West
    | State.West  -> b = State.East
  in
  match l with
  | h::t -> if direction_comp (State.direction_of h) (State.direction_of p) 
    then h 
    else get_partner p t
  | [] -> failwith "No Partner Found"

let points (b : Bid.t) t v = 
  let last_bid = 
    match Bid.last_bid b with 
    | Some bid -> bid | None -> failwith "invalid bid" in
  let eff_bid = get_last_bid (Bid.previous_bids b) in
  let p1 = match eff_bid with (a,b) -> a in
  let p2 = get_partner p1 (State.players_of (Bid.game_of b)) in
  let modifier = get_modifier last_bid eff_bid in
  let tw = tricks_won t p1 p2 in
  let n_bid = get_num_of_bid (match last_bid with (a,b) -> b) in
  let suit_mod = get_suit (match last_bid with (a,b) -> b) in
  if n_bid >= tw then decl_points n_bid tw modifier suit_mod v 
  else (penalty_points n_bid tw modifier v, 0)