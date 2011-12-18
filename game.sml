structure Game :> GAME =
struct
  open Types


   val roboplat = Graphics.requireimage "media/graphics/roboplat.png"
   val roboplatrecording = Graphics.requireimage "media/graphics/roboplatrecording.png"
   val bottombooster = Graphics.requireimage "media/graphics/bottombooster.png"
   val leftbooster = Graphics.requireimage "media/graphics/leftbooster.png"
   val rightbooster = Graphics.requireimage "media/graphics/rightbooster.png"
   val duderight = Graphics.requireimage "media/graphics/duderight.png"
   val dudeleft = Graphics.requireimage "media/graphics/dudeleft.png"
   val playbutton = Graphics.requireimage "media/graphics/playbutton.png"
   val playbuttoninactive = Graphics.requireimage "media/graphics/playbuttoninactive.png"


  open InitWorld

  exception CanJump

  fun canjump b = 
      let val p = B.Body.get_position b
          val SOME fixture = B.Body.get_fixtures b
          val (id, _) = B.Fixture.get_data fixture
          open B.Contact
          fun checkcontact c = 
              if is_touching c
              then
                  let val m = get_manifold c
                      val (fa, fb) = get_fixtures c
                      val ((ida, _), (idb, _)) = (B.Fixture.get_data fa,
                                                  B.Fixture.get_data fb)
                      open BDDTypes
                      val n = #local_normal m
                  in case (ida = id, idb = id, #typ m) of
                         (true, _, E_FaceA) => 
                            if BDDMath.dot2(n, BDDMath.vec2 (0.0, ~1.0)) > 0.0
                            then raise CanJump
                            else ()
                       | (true, _, E_FaceB) => 
                            if BDDMath.dot2(n, BDDMath.vec2 (0.0, 1.0)) > 0.0
                            then raise CanJump
                            else ()
                       | (false, true, E_FaceA) => 
                            if BDDMath.dot2(n, BDDMath.vec2 (0.0, 1.0)) > 0.0
                            then raise CanJump
                            else ()
                       | (false, true, E_FaceB) => 
                            if BDDMath.dot2(n, BDDMath.vec2 (0.0, ~1.0)) > 0.0
                            then raise CanJump
                            else ()
                       | _ => ()

                  end
              else ()
              
          val r = (BDDOps.oapp
                      B.Contact.get_next
                      checkcontact
                      (B.World.get_contact_list world);
                   false)
              handle CanJump => true
      in r
      end


(*
  The box2d world has as its origin the center of the screen.
*)
  fun worldToScreen (v : BDDMath.vec2) : int * int =
      let open BDDMath
          val (xw, yw) = (vec2x v, vec2y v)
          open Real
          val x = (fromInt pixelsPerMeter) *
                  (xw + (meter_width / 2.0))
          val y = (fromInt pixelsPerMeter) *
                  (~yw + (meter_height / 2.0))
          val (xi, yi) = (round x, round y)
      in
          (xi, yi)
      end

  val initstate = ()
  
  fun initscreen screen =
  (
    SDL.flip screen
  )

  fun applyevent {bottom, left, right} BottomOn =
      (bottom := true)
    | applyevent {bottom, left, right} BottomOff =
      (bottom := false)
    | applyevent {bottom, left, right} LeftOn =
      (left := true)
    | applyevent {bottom, left, right} LeftOff =
      (left := false)
    | applyevent {bottom, left, right} RightOn =
      (right := true)
    | applyevent {bottom, left, right} RightOff =
      (right := false)


  fun doreplay NotPlaying = ()
    | doreplay (Playing start) =
      let val dt = Time.-(Time.now (), start)
          val stillalive = ref false


          fun applyevents bst nil = nil
            | applyevents bst (lst as ((t, e)::es)) =
              if (stillalive := true; Time.>(dt, t))
              then (applyevent bst e; applyevents bst es)
              else lst
               
      in
          Util.for 0 (number_of_rps - 1) (fn i =>
           let val rp = Array.sub (rparray, i)
               val rpboosters = Array.sub (rpboosterarray, i)
               val {bottom, left, right} = rpboosters
               val {events, remaining} = Array.sub (scripts, i)
           in remaining := (applyevents rpboosters (!remaining))
           end);
          if not (!stillalive)
          then playback := NotPlaying
          else  ()
      end


  fun applyboosters () =
      let val () = Util.for 0 (number_of_rps - 1) (fn i =>
           let val rp = Array.sub (rparray, i)
               val rpboosters = Array.sub (rpboosterarray, i)
               val {bottom, left, right} = rpboosters
               val () = if !bottom
                        then B.Body.apply_force (rp, BDDMath.vec2 (0.0, 6000.0), zero )
                        else ()
               val () = if !left
                        then B.Body.apply_force (rp, BDDMath.vec2 (2000.0, 0.0), zero )
                        else ()
               val () = if !right
                        then B.Body.apply_force (rp, BDDMath.vec2 (~2000.0, 0.0), zero )
                        else ()
           in () end
                                )
                        
          val {bottom, left, right} = dudeboosters
          val v = B.Body.get_linear_velocity dudebody
          val vx = BDDMath.vec2x v
          val vy = BDDMath.vec2y v
          val mag = BDDMath.vec2length (B.Body.get_linear_velocity dudebody)
          val maxvx = 5.0
          val () = if !left andalso vx > ~maxvx
                   then B.Body.apply_force (dudebody, BDDMath.vec2 (~5.0, 0.0), zero )
                   else ()
          val () = if !right andalso vx < maxvx
                   then B.Body.apply_force (dudebody, BDDMath.vec2 (5.0, 0.0), zero )
                   else ()

          (* do damping by hand. *)
          val () = B.Body.apply_force (dudebody,
                                       BDDMath.vec2 (~3.0 * vx / maxvx, 0.0), zero )

      in () end

  val lasttime = ref (Time.now ())

  fun dophysics () = 
      let val now = Time.now ()
          val diff = Time.-(now, !lasttime)
          val () = lasttime := now
          val millis = IntInf.toString (Time.toMilliseconds (diff))
          val () = B.World.step (world, Time.toReal diff,
                                 10, 10)

 (* For some reason, FLAG_CLEAR_FORCES gets reset to false
    whenever I create an object.
    TODO track this down. is it a bug?*)
          val () = B.World.clear_forces world

      in () end
      

  val white = SDL.color (0w255,0w255,0w255,0w0);

  fun drawbodies screen bl = 
      ( case bl of
            SOME b =>
            let 
                val p = B.Body.get_position b
                val (x, y) = worldToScreen p
                val () =
                    (case B.Body.get_data b of
                         Text {text, width, height} => 
                         let val (x0, y0) = (x - (width div 2),
                                             y - (height div 2)) 
                         in Font.Normal.draw (screen, x0, y0, text) 
                         end 
                       | VerticalLine h =>
                         let val y0 = y - (h div 2)
                             val y1 = y + (h div 2)
                             val () = SDL.drawline (screen, x, y0, x, y1, white)
                         in () end
                       | HorizontalLine w =>
                         let val x0 = x - (w div 2)
                             val x1 = x + (w div 2)
                             val () = SDL.drawline (screen, x0, y, x1, y, white)
                         in () end
                       | RoboPlatform {bottom, left, right} =>
                         let val SOME fix = B.Body.get_fixtures b
                             val (id, fixtype) = B.Fixture.get_data fix
                             val sprite = 
                                 (case (!mode, fixtype) of
                                      (ControlRoboPlatform i,
                                       RoboPlatformFixture j) =>
                                      if i = j then roboplatrecording else roboplat
                                    | _ => roboplat)
                             val () =
                                 if !bottom
                                 then SDL.blitall (bottombooster, screen,
                                                   x - 3, y + 15)
                                 else ()
                             val () =
                                 if !left
                                 then SDL.blitall (leftbooster, screen,
                                                   x - 24, y - 2)
                                 else ()
                             val () =
                                 if !right
                                 then SDL.blitall (rightbooster, screen,
                                                   x + 15, y - 2)
                                 else ()
                             val x1 = x - 17
                             val y1 = y - 15
                             val () = SDL.blitall (sprite, screen, x1, y1)
                         in () end
                       | PlayButton =>
                         (case !playback of
                              Playing _ =>
                              SDL.blitall (playbutton, screen, x - 8, y - 8)
                            | NotPlaying =>
                              SDL.blitall (playbuttoninactive,
                                           screen, x - 8, y - 8)
                         )
                       | Dude (_, dir) =>
                         (case !dir of
                              Right => SDL.blitall (duderight, screen,
                                                        x - 10, y - 15)
                            | Left => SDL.blitall (dudeleft, screen,
                                                   x - 10, y - 15)
                         )
 

                    )
            in drawbodies screen (B.Body.get_next b) end
          | NONE => ()
      )

  fun render screen s =
  (
    SDL.clearsurface (screen, SDL.color (0w00,0w60,0w60,0w60));

    doreplay (!playback);
    applyboosters ();
    dophysics ();
    drawbodies screen (B.World.get_body_list world);
    
    SDL.flip screen
  )

  fun recordEvent e = 
      let val dt = Time.-(Time.now (), !recordingstart)
      in
      recordingevents := ( (dt, e) :: (!recordingevents))
      end

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)

    | keyDown (SDL.SDLK_RIGHT)  (ControlRoboPlatform i) =
      (#left (Array.sub (rpboosterarray, i) ) := true;
       recordEvent LeftOn;
       SOME ())
    | keyDown (SDL.SDLK_LEFT) (ControlRoboPlatform i) =
      (#right (Array.sub (rpboosterarray, i) ) := true;
       recordEvent RightOn;
       SOME ())
    | keyDown (SDL.SDLK_UP)  (ControlRoboPlatform i) = 
      (#bottom (Array.sub (rpboosterarray, i) ) := true;
       recordEvent BottomOn;
       SOME ())

    | keyDown (SDL.SDLK_RIGHT)  ControlDude =
      ((#right dudeboosters) := true;
       dudedir := Right;
       SOME ())
    | keyDown (SDL.SDLK_LEFT) ControlDude =
      ((#left dudeboosters) := true;
       dudedir := Left;
       SOME ())

    | keyDown (SDL.SDLK_UP) ControlDude = 
      (if canjump dudebody
       then B.Body.apply_linear_impulse
                (dudebody,
                 BDDMath.vec2 (0.0, 3.0),
                 zero)
       else ();
       SOME ())

    | keyDown _ s = SOME ()

  fun keyUp (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)

    | keyUp (SDL.SDLK_RIGHT) (ControlRoboPlatform i) =
      (#left (Array.sub (rpboosterarray, i) ) := false;
       recordEvent LeftOff;
       SOME ())
    | keyUp (SDL.SDLK_LEFT)  ((ControlRoboPlatform i)) =
      (#right (Array.sub (rpboosterarray, i) ) := false;
       recordEvent RightOff;
       SOME ())
    | keyUp (SDL.SDLK_UP)  (ControlRoboPlatform i) = 
      (#bottom (Array.sub (rpboosterarray, i) ) := false;
       recordEvent BottomOff;
       SOME ())

    | keyUp (SDL.SDLK_RIGHT)  ControlDude =
      ((#right dudeboosters) := false; SOME ())
    | keyUp (SDL.SDLK_LEFT) ControlDude =
      ((#left dudeboosters) := false; SOME ())

    | keyUp _ s = SOME ()


  fun handle_event (SDL.E_KeyDown {sym=k}) s = keyDown k (!mode)
    | handle_event (SDL.E_KeyUp {sym=k}) s = keyUp k (!mode)
    | handle_event _ s = SOME s


  fun tick s = SOME s
end

structure Main =
struct
  structure S = RunGame (Game)
end
