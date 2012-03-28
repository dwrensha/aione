structure Graphics =
struct
  (* requireimage : string -> surface option *)
  fun requireimage s = 
      case SDL.Image.load s of
            NONE => (print ("couldn't open " ^ s ^ "\n");
                      raise Fail "image not found")
          | SOME p => p

  (* add more images here *)

   val roboplat = requireimage "media/graphics/roboplat.png"
   val roboplatrecording = requireimage "media/graphics/roboplatrecording.png"
   val bottombooster = requireimage "media/graphics/bottombooster.png"
   val leftbooster = requireimage "media/graphics/leftbooster.png"
   val rightbooster = requireimage "media/graphics/rightbooster.png"
   val dude1right = requireimage "media/graphics/dude1right.png"
   val dude2right = requireimage "media/graphics/dude2right.png"
   val dude3right = requireimage "media/graphics/dude3right.png"
   val dude1left = requireimage "media/graphics/dude1left.png"
   val dude2left = requireimage "media/graphics/dude2left.png"
   val dude3left = requireimage "media/graphics/dude3left.png"
   val dudeforward = requireimage "media/graphics/dudeforward.png"
   val duderecording = requireimage "media/graphics/duderecording.png"
   val playbutton = requireimage "media/graphics/playbutton.png"
   val playbuttoninactive = requireimage "media/graphics/playbuttoninactive.png"
   val exitdoor = requireimage "media/graphics/exitdoor.png"
   val victory = requireimage "media/graphics/victory.png"
   val background = requireimage "media/graphics/background.png"





  val (dudeleft, duderight, walk) = 
      let val frames_per_step = 10
          val counter = ref 0
          fun duderight () =
              (case (!counter) div  frames_per_step of
                 0 => dude1right
               | 1  => dude2right
               | 2  => dude3right
               | 3  => dude2right
               | _ => raise Fail "Impossible"
              )
          fun dudeleft () =
              (case (!counter) div  frames_per_step of
                 0 => dude1left
               | 1  => dude2left
               | 2  => dude3left
               | 3  => dude2left
               | _ => raise Fail "Impossible"
              )
          fun walk () =
              (counter := (!counter + 1);
               if (!counter) >= frames_per_step * 4 
               then counter := 0
               else () )
      in 
        (dudeleft, duderight, walk)
      end






end
