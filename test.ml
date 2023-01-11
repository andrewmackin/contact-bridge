(* OUnit test plan:
   Our OUnit test cover the majority of the back end modules and associated
   functions these files include: bid, state, tricks, shuffle, and score
   Of these bid, state and tricks have a variety of white and black box designed
   test to ensure they are correct. Shuffle is not tested directly but it is 
   tested through state which utilizes it. Score was difficult to test due to
   its highly intertwined nature and the decision was made to instead test
   the key helper functions in isolation and later manually test it further.
   The remaining modules are all reliant on the graphics library to a very high
   degree and the decision was made that testing them automatically with ounit
   would cause too much trouble and raise to many issues and so we decided that
   those modules would be tested manually. To limit the chance of introducing
   bugs after testing individual screens we created a very decoupled system
   such that most screens take in miniml information and pass their output to
   main so that all data managment can occur there and the screens remain 
   decoupled.
   What this means for the correctness of the program is that, we have a good
   amount of certainity in the correctness of the backend modules but we cannot
   be as sure about the them being used correctly in the screens although are
   testing leads us to believe there are no errors.
   Additionally the graphics module and its need to connect with other parts
   of the operating system has led to situation where the same build would work
   or break for different group members on different OS's. So proving the 
   graphics module correct in general would be next to impossible.*)
open OUnit2
open State
open Bid
open Tricks 

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same o  rder.
    AI Notice: This was copied directly from test.ml of A2 of CS 3110 FA20. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let dummy_of_test
    (name : string)
    (game : State.t)
    (expected : State.player) =
  name >:: (fun _ -> 
      assert_equal (State.dummy_of game) expected)

let is_dummy_test
    (name : string)
    (player : State.player)
    (game : State.t)
    (expected : bool) =
  name >:: (fun _ ->
      assert_equal (State.is_dummy player game) expected)

let update_dummy_test
    (name : string)
    (game : State.t)
    (expected : State.player) =
  name >:: (fun _ -> 
      assert_equal (State.update_dummy game |> State.dummy_of) expected)

let suit_val_test
    (name : string)
    (card : State.card)
    (expected : int) =
  name >:: (fun _ -> 
      assert_equal (State.suit_val card) expected)

let rank_val_test
    (name : string)
    (card : State.card)
    (expected : int) =
  name >:: (fun _ -> 
      assert_equal (State.rank_val card) expected)

let sort_cards_test
    (name : string)
    (card_lst : State.card list)
    (expected_output : State.card list) =
  name >:: (fun _ ->
      assert_equal (State.sort_cards card_lst) expected_output)

let players_reduce f g game =
  let rec players_reduce_helper acc = function
    | [] -> acc
    | h::t -> players_reduce_helper (f (g h) acc) t
  in players_reduce_helper [] (State.players_of game)

let in_line_cons x y =
  x :: y

let names_lst_of =
  players_reduce in_line_cons State.name_of

let length_of_hand_of player =
  player |> State.hand_of |> List.length

let length_of_hands =
  players_reduce in_line_cons length_of_hand_of

let flatten_hands =
  players_reduce ( @ ) State.hand_of

let new_game_names_test
    (name : string)
    (p : string list)
    (expected : string list) = 
  name >:: (fun _ ->
      assert_equal (State.new_game p |> names_lst_of) expected
        ~cmp: cmp_set_like_lists)

let new_game_hand_length_test
    (name : string)
    (p : string list)
    (expected : int list) =
  name >:: (fun _ ->
      assert_equal (State.new_game p |> length_of_hands) expected)

let compare_sorted_card_lists lst1 lst2 =
  State.sort_cards lst1 = State.sort_cards lst2

let new_game_deck_test
    (name : string)
    (p : string list)
    (expected : State.card list) = 
  name >:: (fun _ ->
      assert_equal (State.new_game p |> flatten_hands) expected
        ~cmp: compare_sorted_card_lists)

let reshuffle_hands_test
    (name : string)
    (game : State.t) =
  name >:: (fun _ -> 
      assert_equal 
        (State.make_deck ()) (State.reshuffle_hands game |> flatten_hands)
        ~cmp: compare_sorted_card_lists)

let play_card_names_test
    (name : string)
    (game : State.t)
    (player : State.player)
    (card : State.card)
    (expected : string list) =
  name >:: (fun _ ->
      assert_equal (play_card game player card |> names_lst_of) expected)

let play_card_hand_length_test
    (name : string)
    (game : State.t)
    (player : State.player)
    (card : State.card)
    (expected : int list) =
  name >:: (fun _ ->
      assert_equal (play_card game player card 
                    |> length_of_hands
                    |> List.rev) expected)

let partner_of_test
    (name : string)
    (players : State.player list)
    (player : State.player)
    (expected : State.player) =
  name >:: (fun _ ->
      assert_equal (State.partner_of players player) expected)

let player_from_direction_test
    (name : string)
    (players : State.player list)
    (dir : State.direction)
    (expected : State.player) =
  name >:: (fun _ ->
      assert_equal (State.player_from_direction players dir) expected)

let next_player_test
    (name : string)
    (players : State.player list)
    (player : State.player)
    (expected : State.player) =
  name >:: (fun _ ->
      assert_equal (State.next_player players player) expected)

let lone_card_lst : State.card list = [(Three, Heart)]
let same_suit_lst_1 : State.card list = [(Three, Club); (Five, Club)]
let same_suit_lst_2 : State.card list = [(King, Spade); (Two, Spade)]
let same_suit_lst_2_sorted : State.card list = [(Two, Spade); (King, Spade)]
let same_rank_lst_1 : State.card list = [(Five, Heart); (Ten, Spade)]
let same_rank_lst_2 : State.card list = [(Queen, Heart); (Queen, Diamond)]
let same_rank_lst_2_sorted : State.card list = 
  [(Queen, Diamond); (Queen, Heart)]
let same_card_lst : State.card list = [(Ace, Diamond); (Ace, Diamond)]
let big_card_lst : State.card list = [(King, Heart); (Two, Diamond); 
                                      (Four, Spade); (Five, Club); 
                                      (Queen, Club); (King, Heart); 
                                      (Two, Club)]
let big_card_lst_sorted : State.card list = [(Two, Club); (Five, Club);
                                             (Queen, Club); (Two, Diamond); 
                                             (King, Heart); (King, Heart); 
                                             (Four, Spade)]
let name_lst_1 = ["Gries"; "Touchdown"; "Clarkson"; "Pollack"]
let state_1 = State.new_game name_lst_1
let state_1_players = State.players_of state_1
let gries = List.hd state_1_players
let touchdown = List.tl state_1_players |> List.hd
let clarkson = List.tl state_1_players |> List.tl |> List.hd
let pollack = State.next_player state_1_players clarkson
let state_2 = State.set_dummy gries state_1
let gries_first = State.hand_of gries |> List.hd
let state_3 = State.play_card state_2 gries gries_first |> State.update_dummy
let gries_2 = State.players_of state_3 
              |> List.find (fun x -> State.name_of x = "Gries") 

let get_first_of lst =
  List.nth lst 0

let get_player_2 state name = 
  state 
  |> players_of 
  |> List.filter (fun x -> name_of x = name) 
  |> get_first_of

let player_1 = get_player_2 state_1 "Gries"
let player_2 = get_player_2 state_1 "Touchdown"
let player_4 = get_player_2 state_1 "Pollack"
let snd_card_of_player_1 = 
  List.nth (get_player_2 state_1 "Gries" |> State.hand_of) 1
let name_lst_2 = names_lst_of state_1

let state_tests = [
  (* Test cards *)
  suit_val_test "Club is 1" (Two, Club) 1;
  suit_val_test "Diamond is 2" (Two, Diamond) 2;
  suit_val_test "Heart is 3" (Two, Heart) 3;
  suit_val_test "Spade is 4" (Two, Spade) 4;
  rank_val_test "Two is 2" (Two, Club) 2;
  rank_val_test "Three is 3" (Three, Club) 3;
  rank_val_test "Four is 4" (Four, Club) 4;
  rank_val_test "Five is 5" (Five, Club) 5;
  rank_val_test "Six is 6" (Six, Club) 6;
  rank_val_test "Seven is 7" (Seven, Club) 7;
  rank_val_test "Eight is 8" (Eight, Club) 8;
  rank_val_test "Nine is 9" (Nine, Club) 9;
  rank_val_test "Ten is 10" (Ten, Club) 10;
  rank_val_test "Jack is 11" (Jack, Club) 11;
  rank_val_test "Queen is 12" (Queen, Club) 12;
  rank_val_test "King is 13" (King, Club) 13;
  rank_val_test "Ace is 14" (Ace, Club) 14;
  (* Test sort_cards *)
  sort_cards_test "Lone card" lone_card_lst lone_card_lst;
  sort_cards_test "Same suit" same_suit_lst_1 same_suit_lst_1;
  sort_cards_test "Same suit, changes order" 
    same_suit_lst_2 same_suit_lst_2_sorted;
  sort_cards_test "Same rank" same_rank_lst_1 same_rank_lst_1;
  sort_cards_test "Same rank, changes order"
    same_rank_lst_2 same_rank_lst_2_sorted;
  sort_cards_test "Many cards" big_card_lst big_card_lst_sorted;
  (* Test new_game *)
  new_game_names_test "Names of initialized game" name_lst_1 name_lst_1;
  new_game_hand_length_test "Even distribution of cards"
    name_lst_1 [13; 13; 13; 13];
  new_game_deck_test "Distribution of cards" name_lst_1 (State.make_deck ());
  (* Test reshuffling hands *)
  reshuffle_hands_test "Basic reshuffle" state_1;
  (* Test play_card *)
  play_card_names_test "Basic"
    state_1 player_1 snd_card_of_player_1 name_lst_2;
  play_card_hand_length_test "One card removed"
    state_1 player_1 snd_card_of_player_1 [12;13;13;13];
  (* test dummy *)
  dummy_of_test "gries" state_2 gries;
  is_dummy_test "gries is dummy" gries state_2 true;
  is_dummy_test "clarson is not dummy" clarkson state_2 false;
  is_dummy_test "there is no dummy" gries state_1 false;
  update_dummy_test "gries plays card" state_3 gries_2;
  update_dummy_test "unchanged" state_2 gries;
  (* test partners *)
  partner_of_test "partner of gries is clarkson" state_1_players gries clarkson;
  partner_of_test "partner of clarkson is gries" state_1_players clarkson gries;
  (* test direction *)
  player_from_direction_test "North is gries" state_1_players State.North gries;
  player_from_direction_test "South is clarkson" 
    state_1_players State.South clarkson;
  (* test next player *)
  next_player_test "gries -> touchdown" state_1_players gries touchdown;
  next_player_test "wraparound, pollack -> gries" state_1_players pollack gries;
]

let bid_option_printer bid_option = 
  match bid_option with 
  | None -> "None"
  | Some (player, bid) -> begin match bid with 
      | Pass -> "Some Pass"
      | Double -> "Some Double"
      | Redouble -> "Some Redouble"
      | Clubs x -> "Some Clubs " ^ string_of_int x
      | Diamonds x -> "Some Diamonds " ^ string_of_int x
      | Hearts x -> "Some Hearts " ^ string_of_int x
      | Spades x -> "Some Spades " ^ string_of_int x
      | NoTrump x -> "Some No Trump" ^ string_of_int x
    end

let last_bid_test name bid_state expected = 
  name >:: (fun _ -> assert_equal expected 
               (Bid.last_bid bid_state) ~printer: (bid_option_printer) )

let declarer_of_test name bid_state expected = 
  name >:: (fun _ -> assert_equal expected (Bid.declarer_of bid_state))

let bid_raises_exn name bid_state player bid =
  name    >:: 
  (fun _ -> assert_raises (InvalidBid bid) 
      (fun () -> Bid.bid bid_state player bid))

let bid_raises_invplayer name bid_state player bid =
  name >:: 
  (fun _ -> assert_raises (InvalidPlayer player)
      (fun () -> Bid.bid bid_state player bid)) 

let previous_bids_test name bid_state expected = 
  name >:: (fun _ -> assert_equal expected 
               (Bid.previous_bids bid_state))

let game = (State.new_game ["Player1";"Player2";"Player3";"Player4"])
let game2 = (State.new_game ["Player10";"Player11";"Player12";"Player13"])
let player_game2 = List.nth (State.players_of game2) 0
let bid_state1 = Bid.new_bidding game 
let players = State.players_of game 
let player1 = List.nth players 0
let player2 = State.next_player players player1 
let player3 = State.next_player players player2
let player4 = State.next_player players player3
let bid_state2 = Bid.bid bid_state1 player2 (Diamonds 3)
let bid_state3 = Bid.bid (Bid.bid (
    Bid.bid bid_state1 player1 (Clubs 1)) player2 (Clubs 2)) player3 (Clubs 4)

let bid_tests = [
  last_bid_test "Empty bid_state has None" bid_state1 None;
  last_bid_test "Bid_state with just Pass is None" 
    (Bid.bid bid_state1 player1 Pass) None; 
  last_bid_test "Bid_state with Clubs 3 should be player, Clubs 3" 
    (Bid.bid bid_state1 player1 (Clubs 3)) (Some (player1, Clubs 3));
  last_bid_test "Bid_state with Clubs 3 added should allow adding NoTrump 3" 
    (Bid.bid bid_state2 player1 (NoTrump 3)) (Some (player1, NoTrump 3));
  last_bid_test "Bid_state with Clubs 3 added should allow addition of Club 4" 
    (Bid.bid bid_state2 player1 (Clubs 4)) (Some (player1, Clubs 4));
  bid_raises_exn "Add Diamonds 3 again raises exn" bid_state2 player1 
    (Diamonds 3);
  bid_raises_exn "Add Clubs 3 raises exn" bid_state2 player1 (Clubs 3);
  bid_raises_exn "Add Diamonds 2 raises exn" bid_state2 player1 (Diamonds 2);
  bid_raises_exn "Can't double when there was no bid" bid_state1 player1 Double;
  bid_raises_exn "Can't double if there was no number and suit bid" 
    (Bid.bid bid_state1 player2 Pass) player1 Double;
  bid_raises_exn "Can't double if the other team hasn't bid a number and suit" 
    (Bid.bid bid_state1 player1 (Diamonds 3)) player1 Double;
  last_bid_test "Allows Double if other team has bid a number and suit" 
    (Bid.bid bid_state2 player1 (Double)) (Some (player2, Diamonds 3));
  bid_raises_exn "Can't redouble if there was no double before it" 
    bid_state1 player1 Redouble;
  last_bid_test "Allows Redouble if other team has bid a double" 
    (Bid.bid (Bid.bid bid_state2 player1 (Double)) player2 Redouble) 
    (Some (player2, Diamonds 3));
  bid_raises_exn "Can't redouble if own team doubled before it" 
    (Bid.bid bid_state2 player1 Double) player1 Redouble;
  bid_raises_invplayer "Can't bet with a player not in game"
    bid_state2 player_game2 (NoTrump 3);
  previous_bids_test "Empty bid_state has None" bid_state1 [];
  previous_bids_test "One pass in bid state" (Bid.bid bid_state1 player1 Pass)
    [(player1, Pass)];
  previous_bids_test "Player2 bidded 3 of diamonds, player1 passed" 
    (Bid.bid bid_state2 player1 Pass) [(player1, Pass); (player2, Diamonds 3)];
  previous_bids_test "Player1 pass, player2 3 of Diamonds" 
    (Bid.bid (Bid.bid bid_state1 player1 Pass) player2 (Diamonds 3)) 
    [(player2, Diamonds 3); (player1, Pass)];
  declarer_of_test "declarer of bidstate2" bid_state2 player2;
  declarer_of_test "declarer of bidstate3" bid_state3 player1;
]

let rec string_of_string_list list str =
  match list with 
  | h :: t -> string_of_string_list t (h ^ str)
  | [] -> str 

let rec string_of_int_list list str =
  match list with 
  | h :: t -> string_of_int_list t (str ^ ";"^ (string_of_int h))
  | [] -> str 

let rec string_of_below below str = 
  match below with 
  | (below1, below2) :: t -> 
    string_of_below t (str ^ ("team1 belowline: " ^ 
                              string_of_int_list below1 "" ^ "team2 belowline: " 
                              ^ string_of_int_list below2 ""))
  | [] -> str 

let score_printer score = 
  let above =
    match score.aboveline with 
    | above1, above2 -> "team1 aboveline: " ^ string_of_int_list above1 "" ^ 
                        "; team2 aboveline" ^ string_of_int_list above2 "; "
  in let below = string_of_below score.belowline "" in 
  above ^ below ^ "team1: " ^ string_of_string_list score.team1 "" ^ "team2: " ^
  string_of_string_list score.team2 "" 

let add_score_test name state expected = 
  name >:: (fun _ -> assert_equal expected (State.game_score_of state) 
               ~printer: score_printer)

let is_vulnerable_test name state player expected = 
  name >:: (fun _ -> assert_equal expected (State.is_vulnerable state player) 
               ~printer: string_of_bool)

let final_points_test name state player expected = 
  name >:: (fun _ -> assert_equal expected (State.final_points state player) 
               ~printer: string_of_int)

let same_team d1 d2 = 
  match d1 with 
  | Some (North) | Some (South) -> if d2 = Some (South) || d2 = Some (North) 
    then true else false
  | Some (East) | Some (West) -> if d2 = Some (East) || d2 = Some (West) 
    then true else false
  | None -> if d2 = None then true else false

let dir_opt_printer d1 = 
  match d1 with 
  | Some North | Some South -> "Some North or South"
  | Some East | Some West -> "Some East or West"
  | None -> "None" 

let rubber_winner_test name gamescore expected = 
  name >:: (fun _ -> assert_equal expected (State.rubber_winner gamescore)
               ~cmp: same_team ~printer: dir_opt_printer)

let state1 = State.new_game ["1";"2";"3";"4"] 
let player1 = state1 |> State.players_of |> List.hd
let player4 = State.next_player (State.players_of state1) player1
let state2 = State.add_round_score state1 (80, 50) player1 
let state3 = State.add_round_score state2 (30, 60) player1
let state4 = State.add_round_score state3 (50, 70) player1
let state5 = State.add_round_score state4 (80, 60) player4
let state6 = State.add_round_score state5 (90, 70) player4
let state7 = State.add_round_score state6 (3, 6) player1 
let state8 = State.add_round_score state7 (-15, 0) player4
let state9 = State.add_round_score state6 (0, 100) player1
let state10 = State.add_round_score state4 (0, 100) player1

let state_score_tests =[
  add_score_test "add score to state1" state2 
    {aboveline = ([80],[]); belowline = [([50], [])]; 
     team1 = ["1";"3"]; team2 = ["2";"4"]};
  add_score_test "add to state2" state3 
    {aboveline = ([30;80],[]); belowline = [([60;50], [])]; 
     team1 = ["1";"3"]; team2 = ["2";"4"]};
  add_score_test "add to state3" state4 
    {aboveline = ([50;30;80],[]); belowline = [([70],[]); ([60;50], [])]; 
     team1 = ["1";"3"]; team2 = ["2";"4"]};
  add_score_test "add to state4" state5 
    {aboveline = ([50;30;80],[80]); belowline = [([70],[60]); ([60;50], [])]; 
     team1 = ["1";"3"]; team2 = ["2";"4"]};
  add_score_test "add to state5" state6 
    {aboveline = ([50;30;80],[90; 80]); 
     belowline = [([70],[70;60]); ([60;50], [])]; 
     team1 = ["1";"3"]; team2 = ["2";"4"]};
  add_score_test "add to state6" state7 
    {aboveline = ([3;50;30;80],[90; 80]); 
     belowline = [([6],[]);([70],[70; 60]); ([60;50], [])]; 
     team1 = ["1";"3"]; team2 = ["2";"4"]};
  add_score_test "test negative" state8 
    {aboveline = ([15;3;50;30;80],[90; 80]); 
     belowline = [([6],[]);([70],[70; 60]); ([60;50], [])]; 
     team1 = ["1";"3"]; team2 = ["2";"4"]};
  is_vulnerable_test "player1 originally not vulnerable" state1 player1 false;
  is_vulnerable_test "player1 not vulnerable state 3" state3 player1 false;
  is_vulnerable_test "player1 is vulnerable state5" state5 player1 true;
  is_vulnerable_test "player4 is not vulnerable state5" state5 player4 false;
  rubber_winner_test "No winner of state1" (State.game_score_of state1) None;
  rubber_winner_test "winner of state9 is player1" (State.game_score_of state9) 
    (Some South);
  final_points_test "player1 points state3" state3 player1 220;
  final_points_test "player4 points state3" state3 player4 0;
  final_points_test "player1 points state 6" state6 player1 340;
  final_points_test "player1 points state 9" state9 player1 940;
  final_points_test "player4 points state 9" state9 player4 300;
  final_points_test "player1 points state10" state10 player1 1140;
]

(*Tricks Test Start*)
let starting_suit_test 
    (name : string)
    (trick : trick)
    (expected_output : State.suit) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (Tricks.starting_suit trick))

let player_cards_test 
    (name : string)
    (trick : trick)
    (expected_output : (State.player * State.card) list) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (player_cards trick) ~cmp:cmp_set_like_lists)

let trump_suit_test 
    (name : string)
    (t : t)
    (expected_output : (State.suit option)) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (trump_suit t))

let past_tricks_test 
    (name : string)
    (t : t)
    (expected_output : (trick list)) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (past_tricks t))

(*Set up for testing*)
(*Helper functions*)
let rec get_card_helper l s cb = 
  match l with
  | h::t -> if(State.suit_of h = s) then h else get_card_helper t s cb
  | [] -> cb

(* [get_card p c] returns a card of the same suit as card c from the hand of 
   player p if there is no card of that suit it returns the first card in the
   hand*)
let get_card p c = 
  get_card_helper (State.hand_of p) 
    (State.suit_of c) (List.hd (State.hand_of p))

let game = (State.new_game ["P1";"P2";"P3";"P4"])
let p1 = (List.nth (State.players_of game) 0)
let p2 = (List.nth (State.players_of game) 1)
let p3 = (List.nth (State.players_of game) 2)
let p4 = (List.nth (State.players_of game) 3)
let c1 = List.hd(State.hand_of p1)
let basic_trick = Tricks.make_trick c1 p1
let c2 = get_card p2 c1
let basic_trick_1 = Tricks.add_card c2 p2 basic_trick
let c3 = get_card p3 c1
let basic_trick_2 = Tricks.add_card c3 p3 basic_trick_1
let c4 = get_card p4 c1
let basic_trick_3 = Tricks.add_card c4 p4 basic_trick_2
let game = State.play_card game p1 c1
let t = Tricks.initalize_tricks (Some State.Spade)
let t1 = Tricks.update_tricks t basic_trick_3

let tricks_tests = [
  starting_suit_test "Starting suit is initalized correctly" basic_trick 
    (State.suit_of c1);
  player_cards_test "Starting player card is initalized correctly" basic_trick
    [(p1,c1)];
  starting_suit_test "Starting suit is unchanged by addition" basic_trick_1 
    (State.suit_of c1);
  player_cards_test "Card is added" basic_trick_1
    [(p1,c1);(p2,c2)];
  starting_suit_test "Starting suit is unchanged by addition" basic_trick_2 
    (State.suit_of c1);
  player_cards_test "Card is added" basic_trick_2
    [(p1,c1);(p2,c2);(p3,c3)];
  starting_suit_test "Starting suit is unchanged by addition" basic_trick_3 
    (State.suit_of c1);
  player_cards_test "Card is added" basic_trick_3
    [(p1,c1);(p2,c2);(p3,c3);(p4,c4)];
  trump_suit_test "Trump suit is set" t (Some State.Spade);
  trump_suit_test "Trump suit is unchanged" t1 (Some State.Spade);
  past_tricks_test "Past tricks starts empty" t [];
  past_tricks_test "Past trick has added trick" t1 [basic_trick_3];
]

(*Trick Test End*)
(*Score Test Start*)
let get_partner_test 
    (name : string)
    (p : State.player)
    (l : State.player list)
    (expected_output : State.player) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (Score.get_partner p l))

let get_modifier_test 
    (name : string)
    (l : ('a * Bid.bid))
    (e : ('a * Bid.bid))
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (Score.get_modifier l e))

let get_last_bid_test 
    (name : string)
    (l : ('a * Bid.bid) list)
    (expected_output : ('a * Bid.bid)) : test = 
  name >:: (fun _ ->
      assert_equal expected_output (Score.get_last_bid l))

let decl_points_test 
    (name : string)
    (b_num : int)
    (w_num : int)
    (modifier : int)
    (suit : int)
    (vul : bool)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output 
        (fst (Score.decl_points b_num w_num modifier suit vul) + 
         snd (Score.decl_points b_num w_num modifier suit vul)) 
        ~printer:string_of_int)

let penalty_points_test 
    (name : string)
    (b_num : int)
    (w_num : int)
    (modifier : int)
    (vul : bool)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      assert_equal expected_output 
        (Score.penalty_points b_num w_num modifier vul) ~printer:string_of_int)

let game = (State.new_game ["P1";"P2";"P3";"P4"])
let players = State.players_of game
let p1 = List.find (fun x -> State.direction_of x = State.North) players
let p2 = List.find (fun x -> State.direction_of x = State.South) players
let p3 = List.find (fun x -> State.direction_of x = State.East) players
let p4 = List.find (fun x -> State.direction_of x = State.West) players
let bid_list_1 = ((),Bid.Clubs 2)::[]
let bid_list_2 = ((),Bid.Pass)::bid_list_1
let bid_list_3 = ((),Bid.Pass)::bid_list_2
let t = Tricks.initalize_tricks (Some State.Club)
let trick1 = Tricks.make_trick

let score_tests = [
  get_partner_test "Partners" p1 players p2;
  get_partner_test "Partners Opposite Order" p2 players p1;
  get_modifier_test "Same last and eff" ((),Bid.Clubs 2) ((),Bid.Clubs 2) 1;
  get_modifier_test "Eff = Double" ((),Bid.Clubs 2) ((),Bid.Double) 2;
  get_modifier_test "Eff = Redouble" ((),Bid.Clubs 2) ((),Bid.Redouble) 4;
  get_last_bid_test "No Pass in front" bid_list_1 ((),Bid.Clubs 2);
  get_last_bid_test "Pass in front" bid_list_2 ((),Bid.Clubs 2);
  get_last_bid_test "Pass's in front" bid_list_3 ((),Bid.Clubs 2);
  (*See: https://en.wikipedia.org/wiki/Bridge_scoring#Rubber_bridge for the 
    example deals and scores used to test here*)
  decl_points_test "" 2 3 1 4 false 100; 
  decl_points_test "" 4 4 1 3 false 120; 
  decl_points_test "" 4 5 2 3 false 390; 
  decl_points_test "" 3 4 1 2 false 80; 
  decl_points_test "" 4 5 2 3 true 490; 
  penalty_points_test "" 5 3 1 true (-200);
]
(*Score Test End*)

let suite = 
  "test suite for MS3110" >::: List.flatten [
    state_tests;
    bid_tests;
    state_score_tests;
    tricks_tests;
    score_tests;
  ]

let _ = run_test_tt_main suite