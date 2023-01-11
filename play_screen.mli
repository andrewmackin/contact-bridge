(**Play screen is the screen seen when the game is during a round*)

(** [tricks_screen b] allows users to play a game of bridge dependent on [b].
    When completed, [tricks_screen b] is [Game_graphics.EndGameScreen t]. *)
val tricks_screen : Bid.t -> Game_graphics.route