structure Game :> GAME =
struct
  open Types


   val roboplat = Graphics.requireimage "media/graphics/roboplat.png"
   val roboplatrecording = Graphics.requireimage "media/graphics/roboplatrecording.png"
   val bottombooster = Graphics.requireimage "media/graphics/bottombooster.png"
   val leftbooster = Graphics.requireimage "media/graphics/leftbooster.png"
   val rightbooster = Graphics.requireimage "media/graphics/rightbooster.png"
   val dude1right = Graphics.requireimage "media/graphics/dude1right.png"
   val dude2right = Graphics.requireimage "media/graphics/dude2right.png"
   val dude3right = Graphics.requireimage "media/graphics/dude3right.png"
   val dude1left = Graphics.requireimage "media/graphics/dude1left.png"
   val dude2left = Graphics.requireimage "media/graphics/dude2left.png"
   val dude3left = Graphics.requireimage "media/graphics/dude3left.png"
   val playbutton = Graphics.requireimage "media/graphics/playbutton.png"
   val playbuttoninactive = Graphics.requireimage "media/graphics/playbuttoninactive.png"
   val exitdoor = Graphics.requireimage "media/graphics/exitdoor.png"
   val victory = Graphics.requireimage "media/graphics/victory.png"
   val background = Graphics.requireimage "media/graphics/background.png"



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
                      (B.World.get_contact_list (!world));
                   false)
              handle CanJump => true
      in r
      end



  val initstate = (Timing.init (); 1)
  
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
    | doreplay Playing =
      let val tc = !tickcounter
          val stillalive = ref false


          fun applyevents bst nil = nil
            | applyevents bst (lst as ((t, e)::es)) =
              if (stillalive := true;  t <= tc)
              then (
                    applyevent bst e;
                    applyevents bst es)
              else lst
               
      in
          Util.for 0 ((GrowArray.length rparray) - 1) (fn i =>
           let val rp = GrowArray.sub rparray i
               val rpboosters = GrowArray.sub rpboosterarray i
               val {bottom, left, right} = rpboosters
               val {events, remaining} = GrowArray.sub scripts i
           in remaining := (applyevents rpboosters (!remaining))
           end);
          if not (!stillalive)
          then playback := NotPlaying
          else  ()
      end


  fun applyboosters () =
      let val () = Util.for 0 ((GrowArray.length rparray) - 1) (fn i =>
           let val rp = GrowArray.sub rparray i
               val rpboosters = GrowArray.sub rpboosterarray i
               val {bottom, left, right} = rpboosters
               val () = if !bottom
                        then B.Body.apply_force (rp, BDDMath.vec2 (0.0, 30000.0), zero )
                        else ()
               val () = if !left
                        then B.Body.apply_force (rp, BDDMath.vec2 (12000.0, 0.0), zero )
                        else ()
               val () = if !right
                        then B.Body.apply_force (rp, BDDMath.vec2 (~12000.0, 0.0), zero )
                        else ()
           in () end
                                )
                        
          val Dude (dudeboosters, dudedir) = B.Body.get_data (!dudebody)
          val {bottom, left, right} = dudeboosters
          val v = B.Body.get_linear_velocity (!dudebody)
          val vx = BDDMath.vec2x v
          val vy = BDDMath.vec2y v
          val mag = BDDMath.vec2length (B.Body.get_linear_velocity (!dudebody))
          val maxvx = 5.0
          val () = if !left andalso vx > ~maxvx
                   then B.Body.apply_force (!dudebody, BDDMath.vec2 (~2.0, 0.0), zero )
                   else ()
          val () = if !right andalso vx < maxvx
                   then B.Body.apply_force (!dudebody, BDDMath.vec2 (2.0, 0.0), zero )
                   else ()

          (* get the sprites to animate *)
          val () = if !left orelse !right then walk () else ()

      in () end

  val seconds_per_tick = 0.01

  fun dophysics () = 
      let 
          (* val millis = IntInf.toString (Time.toMilliseconds (diff)) *)
          
          (* val timestep = Time.toReal diff *)
          val timestep = seconds_per_tick
          val () = B.World.step (!world, timestep,
                                 10, 10)

 (* For some reason, FLAG_CLEAR_FORCES gets reset to false
    whenever I create an object.
    TODO track this down. is it a bug?*)
          val () = B.World.clear_forces (!world)

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
                                                   x - 3, y + 7)
                                 else ()
                             val () =
                                 if !left
                                 then SDL.blitall (leftbooster, screen,
                                                   x - 49, y - 3)
                                 else ()
                             val () =
                                 if !right
                                 then SDL.blitall (rightbooster, screen,
                                                   x + 40, y - 3)
                                 else ()
                             val x1 = x - 40
                             val y1 = y - 8
                             val () = SDL.blitall (sprite, screen, x1, y1)
                         in () end
                       | PlayButton =>
                         (case !playback of
                              Playing =>
                              SDL.blitall (playbutton, screen, x - 8, y - 8)
                            | NotPlaying =>
                              SDL.blitall (playbuttoninactive,
                                           screen, x - 8, y - 8)
                         )
                       | Dude (_, dir) =>
                         (case !dir of
                              Right => SDL.blitall (duderight (), screen,
                                                        x - 10, y - 18)
                            | Left => SDL.blitall (dudeleft (), screen,
                                                   x - 10, y - 18)
                         )
 

                    )
            in drawbodies screen (B.Body.get_next b) end
          | NONE => ()
      )


  val inputstring = ref ""
  val cheating = ref false

  fun render screen (~1) =  (* ~1 means you win *)
      (
       SDL.clearsurface (screen, SDL.color (0w00,0w60,0w60,0w60));
       
       SDL.blitall (victory, screen, 26, 29);
       Font.Huge.draw (screen, 100, 290, "you win");
       SDL.flip screen
      )

  | render screen _ =
  ( (* the usual render function *)
(*    SDL.clearsurface (screen, SDL.color (0w00,0w60,0w60,0w60)); *)

    SDL.blitall (background, screen, 0, 0);
    SDL.blitall (exitdoor, screen, (!exitdoorx) - 8 , (!exitdoory) - 15 );

    drawbodies screen (B.World.get_body_list (!world));
    
(* debugging *)
    Timing.tick(); 
    Font.Normal.draw (screen, 0, 0, "fps: " ^ (Real.toString (Timing.fps() )));
    Font.Normal.draw (screen, 0, 20, "input string: " ^ (!inputstring));
    if !cheating then Font.Normal.draw (screen, 0, 40, "CHEATING") else ();

    SDL.flip screen
  )


  fun recordEvent e = 
      recordingevents :=
        ( (!tickcounter, e) :: (!recordingevents))


  fun keyDown (SDL.SDLK_ESCAPE) _ _ = NONE (* quit the game *)

    | keyDown (SDL.SDLK_RIGHT)  (ControlRoboPlatform i) level =
      (#left (GrowArray.sub rpboosterarray i ) := true;
       recordEvent LeftOn;
       SOME level)
    | keyDown (SDL.SDLK_LEFT) (ControlRoboPlatform i) level =
      (#right (GrowArray.sub rpboosterarray i ) := true;
       recordEvent RightOn;
       SOME level)
    | keyDown (SDL.SDLK_UP)  (ControlRoboPlatform i) level = 
      (#bottom (GrowArray.sub rpboosterarray i ) := true;
       recordEvent BottomOn;
       SOME level)

    | keyDown (SDL.SDLK_DOWN)  (ControlRoboPlatform i) level = 
      (stoprecording i;
       SOME level)

    | keyDown (SDL.SDLK_RIGHT)  ControlDude level =
      let val Dude (dudeboosters, dudedir) = B.Body.get_data (!dudebody)
      in ((#right dudeboosters) := true;
       dudedir := Right;
       SOME level)
      end
    | keyDown (SDL.SDLK_LEFT) ControlDude level =
      let val Dude (dudeboosters, dudedir) = B.Body.get_data (!dudebody)
      in
          ((#left dudeboosters) := true;
           dudedir := Left;
           SOME level)
      end

    | keyDown (SDL.SDLK_UP) ControlDude level = 
      let val p = B.Body.get_position (!dudebody)
          val (x, y) = worldToScreen p
      in if abs (x - (!exitdoorx)) < 15 andalso
            abs (y - (!exitdoory)) < 15
         then (* go to next level *) 
             gotolevel (level + 1)
         else
             (if canjump (!dudebody)
              then B.Body.apply_linear_impulse
                       (!dudebody,
                        BDDMath.vec2 (0.0, 0.7),
                        zero)
              else ();
              SOME level)
      end

    | keyDown SDL.SDLK_RETURN m level = 
      let val newlevel = 
               (case !inputstring of
                    "cheat" => ((cheating := true); SOME level)
                  | mbe_num => 
                    (case Int.fromString mbe_num of
                         SOME lev => gotolevel lev
                       | NONE => SOME level)
               )
      in
       inputstring := "";
       newlevel
      end
    | keyDown k m level = 
      (inputstring := ((!inputstring) ^ (SDL.sdlktos k));
       if String.size (!inputstring) > 40
       then inputstring := ""
       else ();
       SOME level
      )

  fun keyUp (SDL.SDLK_ESCAPE) _ _ = NONE (* quit the game *)

    | keyUp (SDL.SDLK_RIGHT) (ControlRoboPlatform i) level =
      (#left (GrowArray.sub rpboosterarray i ) := false;
       recordEvent LeftOff;
       SOME level)
    | keyUp (SDL.SDLK_LEFT)  ((ControlRoboPlatform i)) level =
      (#right (GrowArray.sub rpboosterarray i ) := false;
       recordEvent RightOff;
       SOME level)
    | keyUp (SDL.SDLK_UP)  (ControlRoboPlatform i) level =
      (#bottom (GrowArray.sub rpboosterarray i ) := false;
       recordEvent BottomOff;
       SOME level)

    | keyUp (SDL.SDLK_RIGHT)  ControlDude level =
      let val Dude (dudeboosters, dudedir) = B.Body.get_data (!dudebody)
      in
      ((#right dudeboosters) := false; SOME level)
      end
    | keyUp (SDL.SDLK_LEFT) ControlDude level =
      let val Dude (dudeboosters, dudedir) = B.Body.get_data (!dudebody)
      in
      ((#left dudeboosters) := false; SOME level)
      end

    | keyUp _ s level = SOME level


  fun handle_event SDL.E_Quit _ = NONE

    (* ~1 means you win *)
    | handle_event (SDL.E_KeyDown {sym=SDL.SDLK_SPACE}) (~1) = NONE
    | handle_event (SDL.E_KeyDown {sym=SDL.SDLK_ESCAPE}) (~1) = NONE

    | handle_event (SDL.E_KeyDown {sym=k}) level = keyDown k (!mode) level
    | handle_event (SDL.E_KeyUp {sym=k}) level = keyUp k (!mode) level
    | handle_event _ level = SOME level


  fun tick (~1) = SOME (~1)
    | tick s = 
      (doreplay (!playback);
       applyboosters ();
       dophysics ();
       tickcounter := (!tickcounter) + 1;
       SOME s
      )
      
end

structure Main =
struct
  structure S = RunGame (Game)
end
