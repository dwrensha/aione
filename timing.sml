signature TIMING = 
sig
  val dt : Time.time ref 
  val tick : unit -> Time.time
  val init : unit -> unit
  val fps : unit -> real
end

structure Timing :> TIMING = 
struct


  fun init () = ()
       
  val dt = ref Time.zeroTime


  (* Implement a circular buffer to keep track of previous timesteps. *)

  val num_steps = 50;


  val steps = Array.array (num_steps, Time.zeroTime)

  val index = ref 0 

  val totaltime = ref Time.zeroTime

  val lasttime = ref (Time.now ())

  fun tick () =
      let val t = Time.now ()
          val step = Time.-(t, (!lasttime))
          val lastindex = (!index)
          val () = index := Int.mod ((!index) + 1, num_steps)
          val laststep = Array.sub (steps, lastindex)
          val oldstep = Array.sub (steps, !index)
          open Time
          val () = totaltime := ((!totaltime) + step - oldstep)

          val () = Array.update (steps, !index, step)
          val () = lasttime := t
      in step end

(*
  fun fps () = (Real.realFloor ((Real.fromInt num_steps) /
                           ( Time.toReal (!totaltime))))
*)
  fun fps () = ((Real.fromInt num_steps) /
                ( Time.toReal (!totaltime)))

end
