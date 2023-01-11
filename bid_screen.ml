open Colors
open Game_graphics

type t = label list

(**[east_color] is the Graphics color used for the player to the east,
   [north_color] is the color used for the player to the north, [south_color]
   is the color used for the player to the south, and [west_color] is the color
   used for the player to the west.*)
let east_color = light_pink
let north_color = sky_blue
let west_color = light_green
let south_color = light_purple

(**[draw_label_lst lst] uses Game_graphics.smart_draw to draw the list [lst].*)
let rec draw_label_lst = function 
  | [] -> () 
  | h :: t -> smart_draw_label h; draw_label_lst t 

(**[make_lbl name x y w h color bid] makes a label with name [name], centered at
   (x,y), width [w], height [h], color [color], route [bid], and no card. *)
let make_lbl name x y w h color bid = 
  ({ name = name; x = x; y = y; w = w; h =h;
     color = color; route = bid; card = None; image = ref None})

(**[process_click_bid_screen mouse_x mouse_y bid_labels] is a
   Game_graphics.route option that corresponds to the result of a click on the 
   bid_screen. *)
let process_click_bid_screen mouse_x mouse_y bid_labels= 
  let rec make_label_lst bls lst = 
    match bls with 
    | [] -> lst 
    | l :: t -> make_label_lst t (l :: lst) 
  in let labels = make_label_lst bid_labels [] in 
  let empty_label = 
    {name = ""; x = 0; y = 0; w = 50; h = 50; 
     color = grey ; route = None; card = None; image = ref None} in 
  let clicked_label = 
    Game_graphics.process_click mouse_x mouse_y labels empty_label in 
  clicked_label.route 

(**[assign_colors players] is a list of players and the colors they are 
   associated  with in the current game. *)
let assign_colors players = 
  let rec assign_helper plst  lst = 
    match plst with 
    | [] -> lst 
    | h :: t -> let direction = State.direction_of h in  
      let color = if direction = North then north_color else 
        if direction = East
        then east_color else if direction = South then south_color 
        else west_color in assign_helper t ((h , color):: lst) 
  in assign_helper players []

(**[make_lbl_row x y w h color suit] is a list of Game_graphics.label that
   that represents a row of seven buttons with the first button centered at
   (x,y) and each button having a width of [w], a height of [h] and a color of
   [color]. Each of the buttons will represent a bid in the suit [suit], 
   starting from 1 and counting up to 7.*)
let make_lbl_row x y w h color suit = 
  let rec make_lbl_row_helper x1 y1 counter lst = 
    if counter > 7 then lst else 
      let label =  match suit with 
        | Bid.Clubs x -> make_lbl (string_of_int counter) x1 y1 w h color 
                           (Some (Bid (Bid.Clubs counter)))
        | Bid.Diamonds x -> make_lbl (string_of_int counter) x1 y1 w h color 
                              (Some (Bid (Bid.Diamonds counter)))
        | Bid.Spades x -> make_lbl (string_of_int counter) x1 y1 w h color 
                            (Some (Bid (Bid.Spades counter)))
        | Bid.Hearts x -> make_lbl (string_of_int counter) x1 y1 w h color 
                            (Some (Bid (Bid.Hearts counter)))
        | Bid.NoTrump x ->make_lbl (string_of_int counter) x1 y1 w h color 
                            (Some (Bid (Bid.NoTrump counter)))
        | Bid.Pass | Bid.Double | Bid.Redouble -> failwith "Should not occur"
      in make_lbl_row_helper (x1 + w) y1 (counter + 1) (label :: lst)
  in make_lbl_row_helper x y 1 []


(**[last_bid player bids] is the last bid of the player [player] in the list 
   [bids]. *)
let last_bid player bids = 
  try Some (List.assoc player bids)
  with 
  | Not_found -> None 

(**[string_of_bid_opt bid_opt] is a string representation of a bid option. *)
let string_of_bid_opt bid_opt = 
  match bid_opt with 
  | None -> ""
  | Some bid -> match bid with 
    | Bid.Clubs x -> "Clubs " ^ string_of_int x
    | Bid.Diamonds x -> "Diamonds " ^ string_of_int x
    | Bid.Hearts x -> "Hearts " ^ string_of_int x
    | Bid.Spades x -> "Spades " ^ string_of_int x
    | Bid.NoTrump x -> "No trump " ^ string_of_int x
    | Bid.Pass -> "Pass"
    | Bid.Double -> "Double"
    | Bid.Redouble -> "Redouble"

(**[add_player_bid last_bid x_pos y_pos w h color labels] adds a the bid 
   [last_bid] to the list [labels] after converting it to a label with 
   a message consisting of [string_of_bid_opt last_bid], centered at [x_pos]
   [y_pos], with width [w], height [h], color [color], no route and no card. *)
let add_player_bid last_bid x_pos y_pos w h color labels =
  match last_bid with 
  | None -> labels
  | _ -> (make_lbl (string_of_bid_opt last_bid) x_pos y_pos w h color None) 
         :: labels

(**[draw_players_bids x_inc y_inc players player bids] is a list of labels 
   representing the players in a game and their most recent bids. *)
let draw_players_bids x_inc y_inc players player bids = 
  let bid_lst = Bid.previous_bids bids
  in let player_colors =  assign_colors players
  in let second_player = State.next_player players player 
  in let third_player = State.next_player players second_player 
  in let last_player = State.next_player players third_player 
  in let second_player_bid = last_bid second_player bid_lst
  in let third_player_bid = last_bid third_player bid_lst
  in let last_player_bid = last_bid last_player bid_lst
  in let player_names = 
       [make_lbl (State.name_of second_player) (x_inc * 2) (y_inc * 14) 
          (x_inc * 3) y_inc (List.assoc second_player player_colors) None;
        make_lbl (State.name_of third_player ^ " (Your team member)") 
          (x_inc * 12) (y_inc * 22) (x_inc * 8)
          y_inc (List.assoc third_player player_colors) None;
        make_lbl (State.name_of last_player) (x_inc * 22) (y_inc * 14)
          (x_inc * 3) y_inc (List.assoc last_player player_colors) None;]
  in let l_scnd_bids = add_player_bid second_player_bid (x_inc *2) 
         (y_inc * 12) (x_inc * 3) y_inc (List.assoc second_player player_colors)
         player_names
  in let l_thrd_bids = add_player_bid third_player_bid (x_inc * 12) 
         (y_inc * 20) (x_inc * 3) y_inc (List.assoc third_player player_colors) 
         l_scnd_bids
  in add_player_bid last_player_bid (x_inc * 22) (y_inc * 12) (x_inc * 3)
    y_inc (List.assoc last_player player_colors) l_thrd_bids

(**[bid_screen_labels x_inc center_y y_inc player_name center_x current_player
   _color grid_left grid_bottom] is a list of labels that represent the double,
   pass, and redouble bids, as well as the clubs, diamonds, hearts, spades, and
   notrump heading. It also displays whose turn it is and includes a label which
   says "your cards:" *)
let bid_screen_labels x_inc center_y y_inc player_name center_x 
    current_player_color grid_left grid_bottom = 
  [
    make_lbl "Double" (x_inc * 9) center_y (x_inc * 2) 
      y_inc light_grey (Some (Bid Bid.Double));
    make_lbl ("It is " ^ player_name ^ "'s turn!") center_x (y_inc * 15) 
      (x_inc * 6) (y_inc * 3) current_player_color None;
    make_lbl "Pass" center_x center_y (x_inc * 2) y_inc light_grey 
      (Some (Bid Bid.Pass));
    make_lbl "Redouble" (x_inc * 15) center_y (x_inc * 2) 
      y_inc light_grey (Some (Bid Bid.Redouble));
    make_lbl "Clubs" grid_left (grid_bottom + 4 * y_inc)
      (x_inc * 2) y_inc grey None ;
    make_lbl "Diamonds" grid_left 
      (grid_bottom + 3 * y_inc) (x_inc * 2) y_inc grey None ;
    make_lbl "Hearts" grid_left (grid_bottom + 2 * y_inc)
      (x_inc * 2) y_inc grey None ;
    make_lbl "Spades" grid_left (grid_bottom + y_inc)
      (x_inc * 2) y_inc grey None ;
    make_lbl "No Trump" grid_left grid_bottom 
      (x_inc * 2) y_inc grey None ;
    make_lbl "Your cards:" (x_inc * 3) (grid_bottom - y_inc)
      (x_inc * 3) y_inc current_player_color None;
  ]

(**[make_grid x_inc y_inc grid_left grid_bottom] is a list of labels that 
   corresponds to a grid of labels with the bottom left label centered at 
   [grid_left],[grid_bottom]. The grid consists of labels from 1 to 7 of Clubs,
   Diamonds, Hearts, Spades, and NoTrump. [x_inc] and [y_inc] are constants used
   to determine the size of the buttons.*)
let make_grid x_inc y_inc grid_left grid_bottom =
  let clubs = make_lbl_row (grid_left + x_inc * 2) (grid_bottom + 4 * y_inc) 
      x_inc y_inc light_grey (Bid.Clubs 1)
  in let diamonds = make_lbl_row (grid_left + x_inc * 2) 
         (grid_bottom + 3 * y_inc) x_inc y_inc light_grey (Bid.Diamonds 1)
  in let hearts = make_lbl_row (grid_left + x_inc * 2) 
         (grid_bottom + 2 * y_inc) x_inc y_inc light_grey (Bid.Hearts 1)
  in let spades = make_lbl_row (grid_left + x_inc * 2) 
         (grid_bottom + y_inc) x_inc y_inc light_grey (Bid.Spades 1)
  in let notrump = make_lbl_row (grid_left + x_inc * 2) 
         (grid_bottom) x_inc y_inc light_grey (Bid.NoTrump 1)
  in clubs @ diamonds @ hearts @ spades @ notrump

(**[wait_click_bid_screen x y labels bids cards player] waits for clicks in a 
   window that has dimensions (x, y), the list [labels] of labels on the screen,
   [bids] is the bid_state of the screen, [player] is the player that is
   currently bidding, and [cards] is their hand. *)
let rec wait_click_bid_screen x y labels bids cards player = 
  let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Mouse_motion] 
  in if s.button 
  then let bid = 
         (process_click_bid_screen s.Graphics.mouse_x s.Graphics.mouse_y labels)
    in match bid with 
    | None -> wait_click_bid_screen x y labels bids cards player
    | Some (Bid bid) -> bid
    | Some (_) -> wait_click_bid_screen x y labels bids cards player
  else if Graphics.size_x () <> x || Graphics.size_y () <> y 
  then bid_screen bids cards player
  else wait_click_bid_screen x y labels bids cards player

and bid_screen bids cards player : Bid.bid = 
  let player_name = State.name_of player 
  in let players = bids |> Bid.game_of |> State.players_of 
  in let player_colors =  assign_colors players
  in let current_player_color = List.assoc player player_colors 
  in let center_x = (Graphics.size_x ()) / 2 
  in let y_inc = (Graphics.size_y ()) / 24 
  in let x_inc = (Graphics.size_x ()) / 24 
  in let center_y = ( Graphics.size_y () ) / 2 
  in let grid_left = x_inc * 7 
  in let grid_bottom = y_inc * 7 
  in let board_labels = bid_screen_labels x_inc center_y y_inc player_name 
         center_x current_player_color grid_left grid_bottom
  in let grid = make_grid x_inc y_inc grid_left grid_bottom
  in let cards_labels = Game_graphics.label_cards cards (x_inc * 2) 
         (y_inc * 3) (x_inc + 10) 5 (y_inc * 4)
  in let players_and_bids = draw_players_bids x_inc y_inc players player bids
  in let labels = (cards_labels @ grid @ players_and_bids @ board_labels) in 
  Graphics.clear_graph (); draw_label_lst labels;
  wait_click_bid_screen (Graphics.size_x ()) (Graphics.size_y ())
    labels bids cards player


