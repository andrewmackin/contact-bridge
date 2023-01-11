type route = 
  | StartScreen
  | RulesScreen
  | BidScreen
  | TricksScreen
  | ScoreScreen
  | TurnScreen
  | EndGameScreen of Tricks.t option
  | EndScreen
  | Bid of Bid.bid
  | Area of label
  (**[label] is a type repersenting a box centered at (x,y) with width w and 
     height h. It contains a string, color, and screen for drawing and 
     proccesiing clicks*)
and label = {
  name : string;
  x : int;
  y : int;
  w : int;
  h : int;
  color : Graphics.color;
  image : (Graphics.image option) ref;
  route : route option;
  card : State.card option
}

type pos = Bottom | Center | Top

(**[make_label x0 y0 w h color word] makes a button. The button's 
   bottom left corner is at the point [(x0, y0)], it has a width [w] and a 
   height [h]. The button is outlined in black and the inside is filled with 
   [color]. [word] is centered in the button and written in black. *)
let make_label text_pos word x0 y0 w h color = 
  let (xi, yi)= Graphics.current_point () in
  Graphics.set_color color;
  Graphics.fill_rect x0 y0 w h; 
  Graphics.set_color Colors.black; 
  let x1 = x0 + w in 
  let y1 = y0 + h in
  Graphics.moveto x0 y0; Graphics.lineto x0 y1; Graphics.lineto x1 y1; 
  Graphics.lineto x1 y0; Graphics.lineto x0 y0;
  let (wordlen, wordheight) = Graphics.text_size word in
  let center_word_x = x0 + w / 2 - wordlen / 2 in
  if text_pos = Center then
    Graphics.moveto center_word_x (y0 + h / 2 - wordheight / 2)
  else if text_pos = Bottom then
    Graphics.moveto center_word_x (y0 + 5)
  else 
    Graphics.moveto center_word_x (y0 + h - 15);
  Graphics.draw_string word;
  Graphics.moveto xi yi

let make_center_label = make_label Center

let make_empty_label = make_label Center ""

let in_bounds obj mouse_x mouse_y =
  obj.x <= mouse_x  && obj.x + obj.w >= mouse_x
  && obj.y <= mouse_y && obj.y + obj.h >= mouse_y

(**[process_click x y] is the screen of the label clicked or [default] if no 
   label was clicked *)
let process_click x y labels default =
  try 
    (List.find 
       (fun l -> (l.x - l.w / 2 < x && x < l.x + l.w / 2) && 
                 (l.y - l.h / 2 < y && y < l.y + l.h / 2)) 
       labels)
  with Not_found -> default

(**[set_background_color color] draws the background of the screen to be [color]
   it clears everything from the screen in doing so*)
let set_background_color color : unit = 
  Graphics.clear_graph ();
  Graphics.set_color color;
  Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
  () 


let short_string_of_rank = State.(function
    | Two   -> "2" | Three -> "3" | Four  -> "4"
    | Five  -> "5" | Six   -> "6" | Seven -> "7"
    | Eight -> "8" | Nine  -> "9" | Ten   -> "10" 
    | Jack  -> "J" | Queen -> "Q" | King  -> "K"
    | Ace   -> "A")

let string_of_suit = State.(function
    | Club    -> "Clubs"
    | Diamond -> "Diamonds"
    | Heart   -> "Hearts"
    | Spade   -> "Spades")

let short_string_of_suit = State.(function
    | Club    -> "C"
    | Diamond -> "D"
    | Heart   -> "H"
    | Spade   -> "S")

let string_of_rank = State.(function
    | Two   -> "Two"   | Three -> "Three" | Four  -> "Four"
    | Five  -> "Five"  | Six   -> "Six"   | Seven -> "Seven"
    | Eight -> "Eight" | Nine  -> "Nine"  | Ten   -> "Ten" 
    | Jack  -> "Jack"  | Queen -> "Queen" | King  -> "King"
    | Ace   -> "Ace")

let get_card_name c = 
  match c with (r,s) ->
    short_string_of_rank r ^ short_string_of_suit s

let label_cards (l : State.card list) x y card_width spacing h = 
  let rec draw_hand_helper l x =
    match l with
    | head::t -> {name = get_card_name head; x = x; y = y; 
                  w = card_width; h = h; color = Colors.light_blue; 
                  route = None;
                  card = Some head; image = ref None}
                 ::(draw_hand_helper t (x + card_width + spacing))
    | [] -> [] in
  draw_hand_helper (State.sort_cards l) (x + card_width / 2)

let width = 114
let height = 178

let img_make f_name = 
  let out = Array.make height (Array.make 0 0) in 
  let channel = open_in f_name in
  try  
    for i = 0 to height - 1 do
      Array.set out i (Array.make width (Graphics.rgb 0 0 0));
      for j = 0 to width - 1 do
        let s = List.map int_of_string
            (List.filter (fun x -> String.length x <> 0) 
               (String.split_on_char ' ' (String.trim (input_line channel)))) in
        match s with
        | a::b::c::[] -> let col = Graphics.rgb a b c in 
          Array.set (Array.get out i) j col;
        | _ -> failwith "Processing Failed"
      done;
    done;
    out |> Graphics.make_image
  with e ->
    close_in_noerr channel;
    raise e

let img_resize w h img = 
  let sample = Graphics.dump_image img in
  let s_w = Float.of_int (Array.length (Array.get sample 0)) in
  let s_h = Float.of_int (Array.length sample) in
  let get_col x y =
    let y_pos = Int.of_float ((Float.of_int y) /. (Float.of_int h) *. s_h) in
    let x_pos = Int.of_float ((Float.of_int x) /. (Float.of_int w) *. s_w) in
    Array.get (Array.get sample y_pos) x_pos 
  in
  let out = Array.make h (Array.make 0 0) in 
  for i = 0 to h - 1 do
    Array.set out i (Array.make w (Graphics.rgb 0 0 0));
    for j = 0 to w - 1 do
      Array.set (Array.get out i) j (get_col j i);
    done;
  done;
  out |> Graphics.make_image

(**[draw_label l] draws the label centered at [(l.x,l.y)] with width [l.w] and 
   height [l.h] and color [l.color] *)
let draw_label is_center l =
  let x, y = if is_center then l.x - l.w / 2, l.y - l.h / 2 else l.x, l.y in 
  make_center_label l.name x y l.w l.h l.color

let draw_card_image is_center l = 
  match !(l.image) with 
  |Some im -> 
    let x,y = if is_center then l.x - l.w / 2, l.y - l.h / 2 else l.x, l.y in
    Graphics.draw_image im x y
  |None -> failwith "Image was None"

let make_card_image is_center l = 
  match l.card with 
  | Some c -> begin match c with
      |(r,s) -> l.image := Some
            (img_make 
               ("assets/rgbarrays/"^short_string_of_rank r
                ^short_string_of_suit s^".txt") 
             |> img_resize l.w l.h); draw_card_image is_center l end
  | None -> failwith "Tried to print something other than a card"

let smart_draw_label_reduce is_center l = 
  if l.card = None then draw_label is_center l else 
  if !(l.image) = None then make_card_image is_center l else
  if Array.length (Graphics.dump_image (Option.get !(l.image))) <> l.h 
  then let f = (fun x -> l.image := Some 
                     (img_resize l.w l.h (Option.get !(l.image)))) in f ();
    draw_card_image is_center l 
  else draw_card_image is_center l 

let smart_draw_label =
  smart_draw_label_reduce true

let smart_draw_label_corner =
  smart_draw_label_reduce false

(**[wait_click_message_screen x y labels ok msg1 msg2 color] waits for a click
   in the message_screen. If there is a click in the area of msg2's label then
   it returns. Otherwise, it checks for a window resize and the continues 
   waiting. *)
let rec wait_click_message_screen x y labels ok msg1 msg2 color= 
  let in_bounds l x y =
    (l.x - l.w / 2 < x && x < l.x + l.w / 2) && 
    (l.y - l.h / 2 < y && y < l.y + l.h / 2)
  in let s = Graphics.wait_next_event 
         [Graphics.Button_down; Graphics.Mouse_motion] in 
  if s.button then 
    if in_bounds ok s.Graphics.mouse_x s.Graphics.mouse_y then () 
    else wait_click_message_screen x y labels ok msg1 msg2 color
  else if Graphics.size_x () <> x || Graphics.size_y () <> y 
  then message_screen msg1 msg2 color 
  else wait_click_message_screen x y labels ok msg1 msg2 color

and message_screen msg1 msg2 color =
  let inc_x = Graphics.size_x () / 24 in 
  let inc_y = Graphics.size_y () / 24 in
  let label1_size = match Graphics.text_size msg1 with (x , y) -> x + inc_x
  in let label2_size = match Graphics.text_size msg2 with (x, y) -> x + inc_x
  in let lbl1 = {name = msg1; x = (inc_x * 12); y = (inc_y * 12); 
                 w = label1_size; h = inc_y; color = color ; 
                 route = None; card = None; image = ref None} in 
  let lbl2 = {name = msg2; x = (inc_x * 12); y = (inc_y * 10); w = label2_size; 
              h = inc_y; color =color; route = None; card = None; image = ref None}
  in   let rec draw_label_lst = function 
      | [] -> () 
      | h :: t -> smart_draw_label h; draw_label_lst t 
  in let labels = lbl1 :: lbl2 :: []
  in Graphics.clear_graph ();
  draw_label_lst labels;
  wait_click_message_screen (Graphics.size_x ()) (Graphics.size_y ()) 
    labels lbl2 msg1 msg2 color