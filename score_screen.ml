open Game_graphics
open Colors

let make_lbl name x y w h color route= 
  ({ name = name; x = x; y = y; w = w; h =h;
     color = color; route = route; card = None; image = ref None})

let rec draw_labels labels = 
  match labels with 
  | h :: t -> Game_graphics.smart_draw_label h; draw_labels t 
  | [] -> ()

let process_click_score_screen mouse_x mouse_y labels = 
  let empty_label = 
    {name = ""; x = 0; y = 0; w = 50; h = 50; 
     color = red; route = None; card = None; image = ref None} in 
  let clicked_label = Game_graphics.process_click mouse_x mouse_y labels 
      empty_label 
  in clicked_label.route

let write_word word center_x center_y = 
  let (x, y ) = Graphics.text_size word 
  in Graphics.moveto (center_x - x/2) (center_y - y/2);
  Graphics.set_color black;
  Graphics.draw_string word

let rec draw_dotted_line start_x start_y = 
  Graphics.moveto start_x start_y ;
  if start_x + 5 < Graphics.size_x () then 
    (Graphics.lineto (start_x + 5) start_y; 
     draw_dotted_line (start_x + (10)) start_y )
  else Graphics.lineto (start_x + 5) (start_y)

let rec print_down list center_x start_y =
  let inc_y = Graphics.size_y () / 24 in 
  match list with 
  | h :: t -> begin write_word (string_of_int h) center_x start_y;
      print_down t center_x (start_y - inc_y) end
  | [] -> start_y + inc_y

let rec print_below_line scores start_y = 
  let inc_y = Graphics.size_y () / 24 in 
  let inc_x = Graphics.size_x () / 24 in 
  match scores with 
  | (team1, team2) :: t -> begin 
      let team1_y = print_down (List.rev team1) (inc_x * 6) (start_y) in 
      let team2_y = print_down (List.rev team2) (inc_x * 18) (start_y) in 
      if team1_y <= team2_y then (draw_dotted_line 0 (team1_y - (inc_y / 2)); 
                                  print_below_line t (team1_y - inc_y))
      else (draw_dotted_line 0 (team2_y - (inc_y / 2)); 
            print_below_line t (team2_y - inc_y))
    end
  | _ -> ()

let rec print_above_line scores center_x start_y =
  let inc_y = Graphics.size_y () / 24 in 
  match scores with 
  | h :: t -> write_word (string_of_int h) center_x start_y;
    print_above_line t center_x (start_y + inc_y )
  | [] -> ()

let print_scores (team1aboveline, team2aboveline) belowline = 
  let inc_x = Graphics.size_x () / 24 
  in let inc_y = Graphics.size_y () / 24
  in print_above_line team1aboveline (inc_x * 6) (inc_y * 13);
  print_above_line team2aboveline (inc_x * 18) (inc_y * 13);
  print_below_line (List.rev belowline) (inc_y * 11)

let rec wait_click_score_screen x y labels score = 
  let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Mouse_motion] 
  in if s.button then
    let route = process_click_score_screen s.Graphics.mouse_x s.Graphics.mouse_y 
        labels 
    in match route with 
    | None -> wait_click_score_screen x y labels score
    | Some route -> ()
  else if Graphics.size_x () <> x || Graphics.size_y () <> y 
  then score_screen score 
  else wait_click_score_screen x y labels score

and score_screen (game_score : State.game_score) = 
  Graphics.clear_graph ();
  let y_inc = (Graphics.size_y ()) / 24 
  in let x_inc = (Graphics.size_x ()) / 24 
  in let team1player1 = List.hd game_score.team1
  in let team1player2 = List.nth game_score.team1 1
  in let team2player1 = List.hd game_score.team2
  in let team2player2 = List.nth game_score.team2 1
  in let labels = [
      make_lbl "Team 1 score:" (x_inc * 6) (y_inc * 22) 
        (x_inc * 4) (y_inc) sky_blue None;
      make_lbl "Team 2 score:" (x_inc * 18) (y_inc * 22) 
        (x_inc * 4) (y_inc) sky_blue None;
      make_lbl "Back" (x_inc * 22) (y_inc * 23) (x_inc * 2) (y_inc) grey 
        (Some TricksScreen);
      make_lbl (team1player1 ^ ", " ^ team1player2) 
        (x_inc * 6) (y_inc * 21) (x_inc * 6) (y_inc) light_pink None;
      make_lbl (team2player1^", " ^ team2player2) 
        (x_inc * 18 ) (y_inc * 21) (x_inc * 6) (y_inc) light_pink None;
    ]
  in 
  draw_labels labels;
  Graphics.moveto (x_inc * 12) (y_inc * 24);
  Graphics.set_color black;
  Graphics.lineto (x_inc * 12) 0;
  Graphics.moveto 0 (y_inc * 12);
  Graphics.lineto (x_inc * 24) (y_inc * 12);
  print_scores game_score.aboveline game_score.belowline;
  (wait_click_score_screen (Graphics.size_x ()) 
     (Graphics.size_y ()) labels game_score );
