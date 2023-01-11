open Game_graphics
open Colors

(**[make_lbl name x y w h color bid] makes a label with name [name], centered at
   (x,y), width [w], height [h], color [color], route [bid], and no card. *)
let make_lbl name x y w h color route = 
  {name = name; x = x; y = y; w = w; h = h; color = color; route = route; 
   card = None; image = ref None}

(**[draw_label_lst lst] uses Game_graphics.smart_draw to draw the list [lst].*)
let rec draw_label_lst = function 
  | [] -> () 
  | h :: t -> draw_label true h; draw_label_lst t 

(**[process_click_start_screen mouse_x mouse_y labels] processes a click by 
   returning tje Game_graphics.route option corresponding to the click at 
   [mosue_x] [mouse_y] *)
let process_click_start_screen mouse_x mouse_y labels = 
  let empty_label = 
    {name = ""; x = 0; y = 0; w = 50; h = 50; 
     color = red; route = None; card = None; image = ref None} in 
  let clicked_label = Game_graphics.process_click mouse_x mouse_y labels 
      empty_label 
  in clicked_label.route

(**[wait_click_start_screen x y labels] waits for clicks on the start screen
   and if the click is inside a label that has a route it returns the 
   Game_graphics.route. Otherwise, it waits for more clicks. *)
let rec wait_click_start_screen x y labels  = 
  let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Mouse_motion] 
  in if s.button then
    let route = process_click_start_screen s.Graphics.mouse_x s.Graphics.mouse_y 
        labels 
    in match route with 
    | None -> wait_click_start_screen x y labels
    | Some route -> route
  else if Graphics.size_x () <> x || Graphics.size_y () <> y 
  then start_screen () 
  else wait_click_start_screen x y labels

and start_screen () : route = 
  let size_y = Graphics.size_y () in 
  let size_x = Graphics.size_x () in
  let y_inc = size_y / 24 in 
  let x_inc = size_x / 24 in 
  let labels = [
    make_lbl "Welcome to Thurston Bridge!" (x_inc * 12) (y_inc * 15) (x_inc * 8)
      (y_inc * 2) bridge_green None;
    make_lbl "Start" (x_inc * 12) (y_inc * 13) (x_inc * 2) 
      y_inc bridge_green (Some BidScreen);
    make_lbl "Rules" (x_inc * 12) (y_inc * 11) (x_inc * 2) y_inc bridge_green
      (Some RulesScreen);
  ]
  in  
  Graphics.clear_graph ();
  draw_label_lst labels;
  wait_click_start_screen (Graphics.size_x ()) (Graphics.size_y ()) labels
