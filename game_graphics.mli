(** Module that handles the graphics in a game of bridge. *)

(**The type that represents the screen to be opened. Feel free to add more
   as needed. *)
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
   height h. It contains a string, color, and screen for drawing and proccesiing
   clicks*)
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


(**[draw_label l] draws the label centered at [(l.x,l.y)] with width [l.w] and 
   height [l.h] and color [l.color] *)
val draw_label : bool -> label -> unit

(**[smart_draw_label label] draws a [label] the same way [draw_label label] would
   except it draws a playing card [x] if [l.card] = x. *)
val smart_draw_label : label -> unit

(**[smart_draw_label_corner label] draws [label] the same was [smart_draw_label]\
   would except with the bottom left corner stored in ([l.x],[l.y]). *)
val smart_draw_label_corner : label -> unit

(**[in_bounds label x_pos y_pos] is true if ([x_pos],[y_pos]) is within 
   the area represented by [label] where [l.x],[l.y] is the bottom left corner 
   of the label. *)
val in_bounds : label -> int -> int -> bool

(**[process_click x y] is the screen of the label clicked or [default] if no 
   label was clicked *)
val process_click : int -> int -> label list -> label -> label

(**[set_background_color color] sets the color of the background of a window to
   be [color]. *)
val set_background_color : Graphics.color -> unit

(**[string_of_suit suit] is a string representation of [suit]. For example,
   if [suit] is Clubs it will return "Clubs". *)
val string_of_suit : State.suit -> string

(**[draw_hand l x y card_width spacing h short draw] is a list of labels for the
   cards in [l]. [x] is the x coordinate left edge of the leftmost card. [y] is 
   the y cordinate of the center of the card. [card_width] is the width of each
   individual card. [spacing] is the space inbetween each card. [h] is the 
   height of the cards. If [short] is true the cards will be repersented with
   abbreivated form. If [draw] is true the cards will be drawn on the screen.*)
val label_cards : State.card list -> int -> int -> int -> int -> int -> label list

(**[message_screen msg1 msg2 color] erases what was on a window and draws a 
   screen with a label in the center that displays the message [msg1] and a 
   label below that which displays the message [msg2]. Both labels have color 
   [color]. If a click is detected in the bottom label, the function returns,
   otherwise it waits for other clicks.*)
val message_screen : string -> string -> Graphics.color -> unit