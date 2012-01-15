signature TIMING = 
sig
  val dt : Time.time ref 
  val tick : unit -> Time.time
  val init : unit -> unit
end

structure Timing :> TIMING = 
struct
  open Time

  fun init () = ()
       
  val dt = ref zeroTime

  val lasttime = ref (now ())

  fun tick () =
      let val t = now ()
          val res = t - (!lasttime)
          val () = lasttime := t
      in res end


end
