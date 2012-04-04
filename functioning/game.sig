signature GAME =
sig
  type state
  type screen = SDL.surface
  val initstate : state
  val initscreen : screen -> unit
  val width : int 
  val height : int

  val render : screen -> state -> unit
  val handle_event : SDL.event -> state -> state option


  (* Target frequency of game logic steps. *)
  val ticks_per_second : real

  (* Take one step of game logic. Typically this is much cheaper than
     rendering, particularly because of the cost of SDL.flip, so
     multiple ticks may be taken between renders. *)
  val tick : state -> state option


end
