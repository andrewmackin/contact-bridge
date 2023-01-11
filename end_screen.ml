open Game_graphics
open Colors

let resize_attr factor init = 
  let float_init = float_of_int init in
  int_of_float (factor *. float_init) 

let resize_labels x_factor y_factor =
  let resize_x, resize_y = resize_attr x_factor, resize_attr y_factor in
  List.fold_left (fun acc label -> 
      {label with x = resize_x label.x;
                  y = resize_y label.y;
                  w = resize_x label.w;
                  h = resize_y label.h} :: acc) []

let draw_labels label_lst = 
  Graphics.clear_graph ();
  List.fold_left (fun acc h -> smart_draw_label h; acc) () label_lst

let end_loop score labels =
  let rec end_loop_helper x y curr_labels =
    let size_x = float_of_int (Graphics.size_x ()) in
    let size_y = float_of_int (Graphics.size_y ()) in
    let x_factor, y_factor = size_x /. x, size_y /. y in 
    let s = 
      Graphics.wait_next_event [Graphics.Button_down; Graphics.Mouse_motion] in
    if s.button then
      let (mouse_x, mouse_y) = s.Graphics.mouse_x, s.Graphics.mouse_y in
      let dummy_label = {name = ""; x = 0;y = 0;w = 0;h = 0;route = None;
                         image = ref None; color = bridge_green; card = None} in 
      let l = process_click mouse_x mouse_y labels dummy_label in 
      if l <> dummy_label then
        match l.route with
        | Some ScoreScreen -> 
          Score_screen.score_screen score; draw_labels labels;
          end_loop_helper size_x size_y curr_labels
        | None -> end_loop_helper size_x size_y curr_labels
        | Some _ -> failwith "Impossible"
      else draw_labels labels; end_loop_helper size_x size_y curr_labels
    else if x_factor <> 1. || y_factor <> 1. then
      resize_labels x_factor y_factor curr_labels 
      |> end_loop_helper size_x size_y
    else end_loop_helper size_x size_y curr_labels
  in let float_x = Graphics.size_x () |> float_of_int in
  let float_y = Graphics.size_y () |> float_of_int in
  draw_labels labels;
  end_loop_helper float_x float_y labels

let make_labels winner_name partner_name =
  let win_label = winner_name ^ " and " ^ partner_name ^ " are the winners!" in
  let x_div_24 = Graphics.size_x () / 24 in
  let y_div_24 = Graphics.size_y () / 24 in
  [{name = win_label; 
    x = 12 * x_div_24;
    y = 14 * y_div_24;
    w = 11 * x_div_24;
    h = y_div_24;
    color = bridge_green;
    route = None; image = ref None; card = None};
   {name = "See Score";
    x = 12 * x_div_24;
    y = 11 * y_div_24;
    w = 3 * x_div_24;
    h = y_div_24;
    color = bridge_green;
    route = Some ScoreScreen; image = ref None; card = None}]

let end_screen game winner =
  let winner_name = State.name_of winner in
  let partner_name = State.(partner_of (players_of game) winner |> name_of) in
  let labels = make_labels winner_name partner_name in
  let score = State.game_score_of game in
  ignore (end_loop score labels); ()

