functor RunGame (Game : GAME) =
struct
  val screen = SDL.makescreen (Game.width, Game.height)


  val last_observed_time = ref (Time.zeroTime)
  val last_simulated_time = ref (Time.zeroTime)


  fun option_iterate f s n =
      if n <= 0 then SOME s
      else (case f s of
                NONE => NONE
              | SOME s' => option_iterate f s' (n - 1)
           )


  fun loop s =
      let val new_observed_time = Time.now ()
          val seconds_to_simulate = Time.toReal(Time.-(new_observed_time,
                                                       !last_simulated_time))
          val num_ticks = Int.max(
                           0,
                           Real.round(seconds_to_simulate / Game.seconds_per_tick))
          val () = last_simulated_time :=
                     Time.+(!last_simulated_time,
                            Time.fromReal(
                            Real.fromInt(num_ticks) * Game.seconds_per_tick))
      in
          case option_iterate Game.tick s num_ticks of
              NONE => ()
            | SOME s =>
              (Game.render screen s;
               case SDL.pollevent () of
                   NONE => (SDL.delay 0; loop s)
                 | SOME e => Option.app loop (Game.handle_event e s))
      end

  val () = Game.initscreen screen

  val () = last_simulated_time := (Time.now())
     
  val () = loop Game.initstate
end

