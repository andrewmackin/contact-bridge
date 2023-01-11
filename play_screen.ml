open Colors
open Game_graphics

type t = {
  bid : Bid.t;
  statics : label list; 
  played_cards : label list; 
  dummy_cards : label list;
  dyn_cards : label list; 
  buttons : label list; 
  game : State.t;
  tricks : Tricks.t;
  current_player : int; 
  current_rnd : int;
  current_trick : Tricks.trick
}

let x_div_24 () =
  Graphics.size_x () / 24

let y_div_24 () =
  Graphics.size_y () / 24

let finish_turn = "Finish Turn"
let finish_game = "Finish Game"

(** [extract_card a] is the Area contained in [a]. Raises [Failure] if [a] is
    not an Area. *)
let extract_area = function
  | Some (Area route) -> route
  | Some _ -> failwith "Card routes to a screen"
  | None -> failwith "Card does not have a route"

(**[make_unclickable l] is [l] with the route of every element of [l] to None *)
let make_unclickable =
  List.map (fun lbl -> {lbl with route = None})

let current_player_of t =
  List.nth (State.players_of t.game) t.current_player

(** [invalid_trick_msg] is the message displayed if a user tries to play
    an invalid trick.*)
let invalid_trick_msg = "That is an invalid trick, please try again"

(** [toggle_finish_turn to_on l] is [l] with the route of the 
    finish_turn/finish_game button set to [Some TurnScreen] if [to_on] and None
    otherwise. *)
let toggle_finish_turn to_on =
  List.map 
    (fun x -> 
       {x with route = 
                 if to_on then 
                   if x.name = finish_turn then Some TurnScreen
                   else if x.name = finish_game then Some (EndGameScreen None) 
                   else x.route
                 else None})

let process_clicks_cards mouse_x mouse_y t =
  let (t'', card_played') =
    List.fold_left (fun (t', card_played) h ->
        let clicked_inside = in_bounds h mouse_x mouse_y in
        if h.route = None || not clicked_inside || card_played
        then {t' with dyn_cards = h::t'.dyn_cards}, card_played
        else
          try
            let route = extract_area h.route in
            let label' = {route with name = h.name} in
            let played_card = Option.get h.card in
            let current_player = current_player_of t in

            let trick' =
              Tricks.add_card played_card current_player t.current_trick in
            let state' = State.play_card t.game current_player played_card 
            in
            (*let dyn_cards' = 
              List.filter (fun x -> x.card != h.card) t.dyn_cards in *)
            {t' with played_cards = label'::t'.played_cards;
                     dyn_cards = t'.dyn_cards;
                     buttons = toggle_finish_turn true t.buttons;
                     game = state'; 
                     current_trick = trick'}, true
          with 
          | Tricks.InvalidCardPlayed _ -> 
            message_screen invalid_trick_msg "OK" light_grey; 

            {t' with dyn_cards = h::t'.dyn_cards}, false 
          | Invalid_argument _ -> failwith "Caught")
      ({t with dyn_cards = []}, false) t.dyn_cards in
  if card_played' then {t'' with dyn_cards = make_unclickable t''.dyn_cards}
  else t''

(** [extract_screen o] is the screen contained in o if [o] contains a screen.
    If [o] is [None] then [extract_screen o] is TricksScreen.
    Raises [Failure] is [o] contains an Area. *)
let extract_screen = function
  | Some (Area _) -> failwith "Button routes to an Area"
  | Some screen -> screen
  | None -> TricksScreen

let process_clicks_buttons mouse_x mouse_y buttons =
  let rec clicks_helper = function
    | [] -> TricksScreen
    | h::t -> let clicked_inside = in_bounds h mouse_x mouse_y in
      if clicked_inside then extract_screen h.route 
      else clicks_helper t
  in clicks_helper buttons

let draw_labels_of =
  List.fold_left (fun acc h -> smart_draw_label_corner h; acc) ()

(** [route_hand n h] is [h] with the [route] field changed to an area on
    the screen dependent on [n].
    Requires: 0 <= [n] <= 3.*)
let route_hand num hand =
  let y_div_24 = y_div_24 () in
  let x_div_24 = x_div_24 () in
  let dummy_label = {name = ""; 
                     x = 6 * x_div_24 + 3 * num * x_div_24; 
                     y = 15 * y_div_24 + 1; 
                     w = 3 * x_div_24; 
                     h = 6 * y_div_24; 
                     color = light_blue;
                     route = None; 
                     card = None;
                     image = ref None} in
  List.map (fun c -> 
      {c with route = Some (Area {dummy_label with card = c.card})}) hand

let trump_suit_string = function
  | Some suit -> "The trump suit is " ^ string_of_suit suit
  | None -> "There is no trump suit"

let make_game_title () = 
  let y_div_24 = y_div_24 () in
  let x_div_24 = x_div_24 () in
  {name = "Thurston Bridge"; 
   x = 9 * x_div_24; 
   y = 22 * y_div_24; 
   w = 6 * x_div_24; 
   h = 2 * y_div_24; 
   color = bridge_green; 
   route = None; 
   card = None; 
   image = ref None}

let title_spots_of title_lst x0 y0 lth hgt = 
  let rec spots_of_helper acc x color = function
    | [] -> acc
    | h::t ->
      let new_color = if color = dark_grey then light_grey else dark_grey in
      let lbl = {name = h; x = x; y = y0; w = lth; h = hgt;
                 color = new_color; route = None; card = None; 
                 image = ref None} in
      spots_of_helper (lbl::acc) (x + lth) new_color t
  in spots_of_helper [] x0 dark_grey title_lst

let spots_of = title_spots_of [""; ""; ""; ""]

let player_spots game =
  let y_div_24 = y_div_24 () in
  let x_div_24 = x_div_24 () in
  let card_length = 3 * x_div_24 in
  let card_height = 6 * y_div_24 in
  let left_border = 6 * x_div_24 in
  let spots = spots_of left_border (15 * y_div_24) card_length card_height in
  let title_spots = title_spots_of (game |> State.players_of |> State.names_of)
      left_border (14 * y_div_24) card_length y_div_24 in
  spots @ title_spots

let initialize_statics game trump_suit =
  let y_div_24 = y_div_24 () in
  let x_div_24 = x_div_24 () in
  let game_title = make_game_title () in
  let trump_banner = {game_title with name = trump_suit_string trump_suit;
                                      x = 2 * x_div_24; 
                                      w = 6 * x_div_24; 
                                      h = y_div_24; 
                                      color = light_purple} in
  let your_hand = {trump_banner with name = "Your Hand:";
                                     x = x_div_24 + 5;
                                     y = 21 * y_div_24 / 2;
                                     w = 3 * x_div_24;
                                     color = light_pink} in
  let dummy_string = State.(dummy_of game |> name_of) 
                     ^ " is the Dummy. This is their hand:" in
  let dummys_hand = {your_hand with name = dummy_string;
                                    y = 5 * y_div_24;
                                    w = 11 * x_div_24} in 
  [game_title; trump_banner; your_hand; dummys_hand] @ (player_spots game)

let update_buttons t =
  if t.current_player = 2 && t.current_rnd = 12 then
    List.map (fun b -> 
        if b.name = finish_turn then 
          {b with name = finish_game}
        else b) t.buttons
  else t.buttons

let make_dummy_hand dummy =
  let y_div_24 = y_div_24 () in
  let x_div_24 = x_div_24 () in
  let dummy_hand = State.hand_of dummy in
  label_cards dummy_hand (x_div_24 / 2) (3 * y_div_24 / 2) 
    (3 * x_div_24 / 2) 5 (3 * y_div_24)

let set_round t =
  let game' = State.update_dummy t.game in
  let g_dummy_hand = State.dummy_of game' |> make_dummy_hand in
  if t.current_player = 3 then
    let game'' = Tricks.winner t.tricks t.current_trick 
                 |> State.direction_of
                 |> State.sort_by_direction game' in
    let new_statics = initialize_statics game'' (Bid.trump_suit_of t.bid) in
    let new_trick = Tricks.update_tricks t.tricks t.current_trick in
    {t with statics = new_statics; 
            played_cards = [];
            dummy_cards = g_dummy_hand;
            game = game''; 
            current_player = 0; 
            current_rnd = t.current_rnd + 1;
            current_trick = Tricks.make_empty_trick ();
            tricks = new_trick}
  else 
    {t with dummy_cards = g_dummy_hand;
            buttons = update_buttons t;
            game = game';
            current_player = t.current_player + 1}

let make_graphic_hand hand current = 
  let y_div_24 = y_div_24 () in
  let x_div_24 = x_div_24 () in
  label_cards hand (x_div_24 / 2) (7 * y_div_24) (3 * x_div_24 / 2) 
    5 (3 * y_div_24)
  |> route_hand current

let between_tricks t =
  let t' = set_round t in
  let next_player = current_player_of t' in
  let name = if State.is_dummy next_player t'.game then 
      State.partner_of (State.players_of t'.game) next_player |> State.name_of
    else State.name_of next_player in
  let message = "It is " ^ name ^ "'s turn. Please give them the computer" in 
  message_screen message "Ready" light_grey;
  let hand' = State.hand_of next_player in
  let dyn' = make_graphic_hand hand' t'.current_player
  in
  {t' with dyn_cards = dyn'; 
           buttons = toggle_finish_turn false t'.buttons}

let scaled_to_int factor curr =
  factor *. float_of_int curr |> int_of_float

let rec resize x_ratio y_ratio =
  List.map (fun l -> 
      {l with x = scaled_to_int x_ratio l.x;
              y = scaled_to_int y_ratio l.y;
              w = scaled_to_int x_ratio l.w;
              h = scaled_to_int y_ratio l.h;
              route = match l.route with
                | Some (Area route) -> 
                  Some (Area (resize x_ratio y_ratio [route] |> List.hd))
                | x -> x
      })

let resize_graphics x_ratio y_ratio t = 
  let resize_field = resize x_ratio y_ratio in
  {t with statics = resize_field t.statics;
          played_cards = resize_field t.played_cards;
          dummy_cards = resize_field t.dummy_cards;
          dyn_cards = resize_field t.dyn_cards;
          buttons = resize_field t.buttons}

let draw_all t =
  Graphics.clear_graph ();
  draw_labels_of t.statics;
  draw_labels_of t.played_cards;
  draw_labels_of t.dummy_cards;
  draw_labels_of t.dyn_cards; 
  draw_labels_of t.buttons; ()

let rec wcts_helper x y t =
  let x_float, y_float = float_of_int x, float_of_int y in
  let size_x = float_of_int (Graphics.size_x ()) in
  let size_y = float_of_int (Graphics.size_y ()) in
  let x_factor, y_factor = size_x /. x_float, size_y /. y_float in 
  let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Mouse_motion] 
  in if s.button then
    let mouse_x, mouse_y = s.Graphics.mouse_x, s.Graphics.mouse_y in
    match process_clicks_buttons mouse_x mouse_y t.buttons with
    | TricksScreen -> let t' = process_clicks_cards mouse_x mouse_y t in
      draw_all t'; wcts_helper x y t'
    | TurnScreen -> let t' = between_tricks t in 
      draw_all t'; wcts_helper x y t'
    | RulesScreen -> 
      ignore (Rules_screen.rules_screen TricksScreen); wcts_helper x y t
    | ScoreScreen -> 
      Score_screen.score_screen (State.game_score_of t.game); draw_all t;
      wcts_helper x y t
    | EndGameScreen _ -> EndGameScreen (Some t.tricks)
    | StartScreen | BidScreen | Bid _ | Area _ | EndScreen ->
      failwith "impossible"
  else if x_factor <> 1. || y_factor <> 1. then
    resize_graphics x_factor y_factor t 
    |> wcts_helper (int_of_float size_x) (int_of_float size_y)
  else wcts_helper x y t

let wait_click_tricks_screen t_init =
  draw_all t_init;
  wcts_helper (Graphics.size_x ()) (Graphics.size_y ()) t_init

let make_buttons () =
  let y_div_24 = y_div_24 () in
  let x_div_24 = x_div_24 () in
  let play_button = {name = finish_turn; x = 13 * x_div_24;
                     y = 12 * y_div_24; w = 4 * x_div_24; h = y_div_24; 
                     color = sky_blue; route = None; card = None; 
                     image = ref None} in
  let rules_button = {play_button with name = "Rules"; x = 18 * x_div_24; 
                                       y = 22 * y_div_24; 
                                       w = 3 * x_div_24; 
                                       color = light_purple;
                                       route = Some RulesScreen} in
  let score_button = {play_button with name = "Score"; x = 8 * x_div_24;
                                       w = 2 * x_div_24;
                                       color = sky_blue;
                                       route = Some ScoreScreen} in
  [play_button; rules_button; score_button]

let tricks_screen state = 
  Graphics.clear_graph ();
  let trump_suit = Bid.trump_suit_of state in
  let game = Bid.game_of state in
  let players = State.players_of game in
  let declarer = Bid.declarer_of state in
  let dummy = State.partner_of players declarer in
  let opening_lead = State.next_player players declarer in
  let game' = opening_lead |> State.direction_of 
              |> State.sort_by_direction game |> State.set_dummy dummy in
  let statics = initialize_statics game' trump_suit in
  let dummy_hand = make_dummy_hand dummy in
  let first_hand = make_graphic_hand (State.hand_of opening_lead) 0 in 
  wait_click_tricks_screen {bid = state;
                            statics = statics; 
                            played_cards = [];
                            dummy_cards = dummy_hand; 
                            dyn_cards = first_hand; 
                            buttons = make_buttons (); 
                            game = game'; 
                            current_player = 0;
                            current_rnd = 0;
                            current_trick =  Tricks.make_empty_trick ();
                            tricks = Tricks.initalize_tricks trump_suit}