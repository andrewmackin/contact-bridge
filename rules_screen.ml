open Game_graphics
open Colors

let rules = List.map (String.map (fun x -> if x = '\n' then ' ' else x)) [
    "  Bridge is a trick taking card game. Where each player is dealt 13 cards
and sits at one of the four sides of a table. The players on opposite
sides of the table are partners. A round begins when after being dealt
cards players go around the table in clockwise fashion and make a bid.
After the bid has been determined 13 tricks are played and after that
the round is scored.";
    "";
    "  Bidding goes in clockwise order each player may choose to make a bid,
By naming a number and suit that must be greater in either than the
previous bid. After three players in a row decide to make a bid of Pass.
The bidding is over an the person who made the largest bid gets to go
first in the first trick and their partner becomes a dummy. The dummy
places there cards face up for all to see and the winner of the bid plays
for them in the following tricks.";
    "";
    "  Tricks play out like so, in clockwise order each person plays a card.
Players may only play cards of the suit of the first card unless they
do not have any cards of that suit in which case they may play any card.
The Highest number card wins with the exception that cards of the trump
suit, the suit named in the bid are higher than cards of all other
types. After all 13 tricks are played scoring occurs based on the number
that each team won."
  ]


let draw_line center_col right_col left_col offset_small a l = 
  let () = Graphics.moveto 
      (center_col - (right_col - left_col) / 2 + offset_small) a in
  let () = Graphics.draw_string l in
  a - 14

(*The idea for how to detect a resize event was based upon observations from
  https://discuss.ocaml.org/t/what-ive-found-playing-with-graphics/739*)
let rec wait_click_game_rules d_screen x y labels = 
  let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Mouse_motion] 
  in 
  if s.button then 
    let s = Graphics.wait_next_event [Graphics.Button_up] in
    let screen = Option.get (Game_graphics.process_click 
                               s.Graphics.mouse_x s.Graphics.mouse_y 
                               labels (List.hd labels)).route
    in if screen = RulesScreen then wait_click_game_rules d_screen x y labels
    else screen
  else if (Graphics.size_x ()) <> x || (Graphics.size_y ()) <> y 
  then rules_screen d_screen else wait_click_game_rules d_screen x y labels

(**[game_rules] draws the game rules screen.*)
and rules_screen d_screen = 
  let offset_medium = 35 in
  let offset_small = 10 in
  let center_col = Graphics.size_x () / 2 in 
  let right_col = 7 * Graphics.size_x () / 8 in
  let left_col = Graphics.size_x () / 8 in
  let top_row = 5 * Graphics.size_y () / 6 in
  let bot_row = Graphics.size_y () / 6 in
  Game_graphics.(set_background_color grey);
  let labels = [
    {name = "Rules:"; x = center_col; y = top_row; 
     w = 300; h = 50; color = light_blue; route = Some RulesScreen;
     card = None; image = ref None};
    {name = "Back"; x = left_col; y = top_row + offset_medium;
     w = 100; h = 25; color = light_blue; route = Some d_screen;
     card = None; image = ref None};
    {name = ""; x = center_col; y = 3 * (top_row - bot_row) / 5;
     w = right_col - left_col; h = top_row - bot_row + offset_small; 
     color = light_blue; route = Some RulesScreen;
     card = None; image = ref None}
  ] in
  List.iter (Game_graphics.draw_label true) labels;

  let rec get_space start str w =
    if  String.get str (start + w ) <> ' ' && w <> 0
    then get_space start str (w - 1)
    else w in

  let width = Graphics.size_x () / 8 - 3 in
  let rec rules_helper start str len =
    if start + width < len - 1
    then let w = (get_space start str width) + 1 in 
      (String.sub str start w)::
      rules_helper (start + w) str len
    else (String.sub str start (len - start))::[] in
  let rules_list = List.map (fun x -> rules_helper 0 x (String.length x)) rules 
                   |> List.flatten  in

  Graphics.moveto 
    (center_col - (right_col - left_col) / 2 + offset_small) 
    (top_row - bot_row - offset_medium);
  ignore (List.fold_left 
            (draw_line center_col right_col left_col offset_small) 
            (14 * (top_row - bot_row) / 13) rules_list);
  wait_click_game_rules 
    d_screen (Graphics.size_x ()) (Graphics.size_y ()) labels
