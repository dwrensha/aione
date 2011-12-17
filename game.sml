structure Game :> GAME =
struct
  open Types


   val roboplat = Graphics.requireimage "media/graphics/roboplat.png"
   val bottombooster = Graphics.requireimage "media/graphics/bottombooster.png"
   val leftbooster = Graphics.requireimage "media/graphics/leftbooster.png"
   val rightbooster = Graphics.requireimage "media/graphics/rightbooster.png"
   val duderight = Graphics.requireimage "media/graphics/duderight.png"
   val dudeleft = Graphics.requireimage "media/graphics/dudeleft.png"

  open InitWorld

  exception CanJump

  fun canjump b = 
      let val p = B.Body.get_position b
          val SOME fixture = B.Body.get_fixtures b
          val id = B.Fixture.get_data fixture
          open B.Contact
          fun checkcontact c = 
              if is_touching c
              then
                  let val m = get_manifold c
                      val (fa, fb) = get_fixtures c
                      val (ida, idb) = (B.Fixture.get_data fa,
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

  val initstate = ControlDude
  
  fun initscreen screen =
  (
    SDL.flip screen
  )

  fun applyboosters () =
      let val {bottom, left, right} = rpboosters
          val () = if !bottom
                   then B.Body.apply_force (rp, BDDMath.vec2 (0.0, 30.0), zero )
                   else ()
          val () = if !left
                   then B.Body.apply_force (rp, BDDMath.vec2 (10.0, 0.0), zero )
                   else ()
          val () = if !right
                   then B.Body.apply_force (rp, BDDMath.vec2 (~10.0, 0.0), zero )
                   else ()
                        
          val {bottom, left, right} = dudeboosters
          val () = if !bottom
                   then B.Body.apply_force (dudebody, BDDMath.vec2 (0.0, 30.0), zero )
                   else ()
          val () = if !left
                   then B.Body.apply_force (dudebody, BDDMath.vec2 (~5.0, 0.0), zero )
                   else ()
          val () = if !right
                   then B.Body.apply_force (dudebody, BDDMath.vec2 (5.0, 0.0), zero )
                   else ()
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
                         let 
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
                             val () = SDL.blitall (roboplat, screen, x1, y1)
                         in () end
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

    applyboosters();
    dophysics ();
    drawbodies screen (B.World.get_body_list world);
    
    SDL.flip screen
  )

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown (SDL.SDLK_RIGHT)  ControlRoboPlatform =
      ((#left rpboosters) := true; SOME ControlRoboPlatform)
    | keyDown (SDL.SDLK_LEFT) ControlRoboPlatform =
      ((#right rpboosters) := true; SOME ControlRoboPlatform)
    | keyDown (SDL.SDLK_UP)  ControlRoboPlatform = 
      ((#bottom rpboosters) := true; SOME ControlRoboPlatform)

    | keyDown (SDL.SDLK_RIGHT)  ControlDude =
      ((#right dudeboosters) := true;
       dudedir := Right;
       SOME ControlDude)
    | keyDown (SDL.SDLK_LEFT) ControlDude =
      ((#left dudeboosters) := true;
       dudedir := Left;
       SOME ControlDude)

    | keyDown (SDL.SDLK_UP) ControlDude = 
      (if canjump dudebody
       then B.Body.apply_linear_impulse
                (dudebody,
                 BDDMath.vec2 (0.0, 4.0),
                 zero)
       else ();
       SOME ControlDude)

    | keyDown (SDL.SDLK_f) ControlRoboPlatform = SOME ControlDude
    | keyDown (SDL.SDLK_f) ControlDude = SOME ControlRoboPlatform

    | keyDown _ s = SOME s

  fun keyUp (SDL.SDLK_RIGHT) ControlRoboPlatform =
      ((#left rpboosters) := false; SOME ControlRoboPlatform)
    | keyUp (SDL.SDLK_LEFT)  ControlRoboPlatform =
      ((#right rpboosters) := false; SOME ControlRoboPlatform)
    | keyUp (SDL.SDLK_UP)  ControlRoboPlatform = 
      ((#bottom rpboosters) := false; SOME ControlRoboPlatform)

    | keyUp (SDL.SDLK_RIGHT)  ControlDude =
      ((#right dudeboosters) := false; SOME ControlDude)
    | keyUp (SDL.SDLK_LEFT) ControlDude =
      ((#left dudeboosters) := false; SOME ControlDude)

    | keyUp _ s = SOME s


  fun handle_event (SDL.E_KeyDown {sym=k}) s = keyDown k s
    | handle_event (SDL.E_KeyUp {sym=k}) s = keyUp k s
    | handle_event _ s = SOME s


  fun tick s = SOME s
end

structure Main =
struct
  structure S = RunGame (Game)
end
