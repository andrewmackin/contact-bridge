(* main module for running the game *)

let round_end_screen points declarer = 
  match points with 
  | (above, below) -> if below > 0 
    then Game_graphics.message_screen 
        ("The declarer made contract and recieves " ^ (string_of_int above) 
         ^ " points above the line and " ^ (string_of_int below) ^ 
         " points below the line") "Continue" Colors.light_blue
    else Game_graphics.message_screen 
        ("The declarer didn't make contract and the other team receives " ^ 
         (string_of_int (-above)) ^" points above the line") 
        "Continue" Colors.light_blue

let cornell_names = ["Pollack"; "Lombardi"; "Gries"; "Touchdown"; "Clarkson";
                     "Foster"; "Ezra"; "A.D. White"; "Carl Sagan"; "Bill Nye"; 
                     "W. White"; "Bracy"; "Bingalee"; "Dingalee"; "RBG"]

let choose_4 () =
  Shuffle.shuffle cornell_names
  |> List.fold_left 
    (fun acc x -> if List.length acc = 4 then acc else x::acc) []

let rec update_bids t new_player bid bid_state = 
  if Bid.three_passes t 
  then let message = ("Reshuffling cards... " ^ 
                      (State.name_of new_player) ^ "'s turn") in  
    Game_graphics.message_screen message "Ready" Colors.red;
    let state' = State.reshuffle_hands (Bid.game_of bid_state) in
    let player' = State.player_from_direction 
        (State.players_of state') (State.direction_of new_player) 
    in bid_turns player' (Bid.new_bidding state')

  else between_bids new_player bid_state

(**[between_bids new_player new_bid_state] draws a screen that says which 
   players turn is coming up, and when "Ready" is clicked it starts a turn 
   for that player. *)
and between_bids new_player new_bid_state = 
  let message = "It is " ^ (State.name_of new_player) ^
                "'s turn. Please give them the computer."
  in Game_graphics.message_screen message "Ready" Colors.grey; 
  bid_turns new_player new_bid_state 

(**[bid_turns player bid_state] goes through the bidding turns in a game of 
   bridge and terminates when bidding is over.*)
and bid_turns player bid_state = 
  let state = Bid.game_of bid_state in
  let cards = State.hand_of player in 
  let bid = Bid_screen.bid_screen bid_state cards player in 
  let new_bid_state = try Bid.bid bid_state player bid with 
    | Bid.InvalidBid bid -> 
      Game_graphics.message_screen "That is an invalid bid, please try again" 
        "OK" Colors.red; 
      bid_turns player bid_state
  in let new_player = State.next_player (State.players_of state) player
  in if Bid.three_passes (Bid.previous_bids new_bid_state) 
  then (if Bid.last_bid new_bid_state = None 
        then match (Bid.previous_bids new_bid_state) with 
          | h :: t -> update_bids t new_player bid new_bid_state
          | _ -> failwith "should not occur"
        else new_bid_state )
  else between_bids new_player new_bid_state

and game_loop bid = 
  let screen = Play_screen.tricks_screen bid 
  in let tricks =
       match screen with 
       | EndGameScreen (Some x) -> x 
       | EndGameScreen None -> failwith "No trick passed"
       | _ -> failwith "should only be an EndGameScreen"
  in let new_score = Score.points bid tricks 
         (State.is_vulnerable (Bid.game_of bid) (Bid.declarer_of bid)) 
  in let new_state = State.add_round_score (Bid.game_of bid) new_score 
         (Bid.declarer_of bid) 
  in match State.rubber_winner (State.game_score_of new_state) with 
  | Some x -> 
    List.fold_left 
      (fun acc x -> ((State.final_points new_state x, x) :: acc)) 
      [] (State.players_of new_state)
    |> List.sort (fun (points1,_) (points2,_) -> compare points1 points2)
    |> List.hd
    |> snd
    |> End_screen.end_screen new_state
  | None -> round_end_screen new_score (Bid.declarer_of bid); 
    let new_bid_state = bid_turns (Bid.declarer_of bid) 
        (Bid.new_bidding new_state)
    in game_loop new_bid_state

(**[start_bidding] starts a new game and starts the bidding for it. It returns
   a finished bid_state.  *)
and start_bidding () =
  let state = State.new_game (choose_4 ()) in 
  let players = State.players_of state in
  let bid_state = Bid.new_bidding state in 
  bid_turns (State.player_from_direction players State.North) bid_state

let rec start_graphics screen = 
  match screen with 
  | Game_graphics.BidScreen -> game_loop (start_bidding ()) 
  | Game_graphics.RulesScreen -> 
    start_graphics (Rules_screen.rules_screen Game_graphics.StartScreen)
  | _ -> start_graphics (Start_screen.start_screen ())

let start =  
  Graphics.open_graph "";
  Graphics.set_window_title "Thurston Bridge";
  start_graphics (Start_screen.start_screen ())


