structure Game :> GAME =
struct
  open Types


  (* Constants *)
  val width = 800
  val height = 600
  val pixelsPerMeter = 20
  val meter_height = (Real.fromInt height /
                      Real.fromInt pixelsPerMeter)
  val meter_width = (Real.fromInt width /
                     Real.fromInt pixelsPerMeter)


   val roboplat = Graphics.requireimage "media/graphics/roboplat.png"
   val bottombooster = Graphics.requireimage "media/graphics/bottombooster.png"
   val leftbooster = Graphics.requireimage "media/graphics/leftbooster.png"
   val rightbooster = Graphics.requireimage "media/graphics/rightbooster.png"
   val duderight = Graphics.requireimage "media/graphics/duderight.png"
   val dudeleft = Graphics.requireimage "media/graphics/dudeleft.png"


  
  val gravity = BDDMath.vec2 (0.0, ~10.0) 
  val world = B.World.world (gravity, true)
  val () = B.World.set_auto_clear_forces (world,  true)

  fun new_boosters () = {bottom = ref false,
                         left = ref false,
                         right = ref false}

  fun create_roboplatform (p : BDDMath.vec2)
                          (v : BDDMath.vec2)
                          (mass : real) : B.body = 
      let val pixel_width = 28
          val pixel_height = 28
          val meter_width = (Real.fromInt pixel_width) /
                            (Real.fromInt pixelsPerMeter)
          val meter_height = (Real.fromInt pixel_height) /
                             (Real.fromInt pixelsPerMeter)
          val body = B.World.create_body
                         (world,
                          {typ = B.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = v,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = true,
                           bullet = false,
                           active = true,
                           data = RoboPlatform (new_boosters ()),
                           inertia_scale = 1.0
                         })
          val density = mass / meter_width * meter_height
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  meter_height / 2.0)),
                             (),
                             density)
          val () = B.Fixture.set_restitution (fixture, 0.1)
          val () = B.Fixture.set_friction (fixture, 0.5)
      in body end

  val rp = create_roboplatform (BDDMath.vec2 (0.0, 0.0)) (BDDMath.vec2 (0.0, 0.0)) 1.0
  val RoboPlatform rpboosters = B.Body.get_data rp

  fun create_dude (p : BDDMath.vec2)
                  (v : BDDMath.vec2)
                  (mass : real) : B.body = 
      let val pixel_width = 8
          val pixel_height = 26
          val meter_width = (Real.fromInt pixel_width) /
                            (Real.fromInt pixelsPerMeter)
          val meter_height = (Real.fromInt pixel_height) /
                             (Real.fromInt pixelsPerMeter)
          val body = B.World.create_body
                         (world,
                          {typ = B.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = v,
                           angular_velocity = 0.0,
                           linear_damping = 1.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = true,
                           bullet = false,
                           active = true,
                           data = Dude (new_boosters(), ref Right),
                           inertia_scale = 1.0
                         })
          val density = mass / meter_width * meter_height
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  meter_height / 2.0)),
                             (),
                             density)
          val () = B.Fixture.set_restitution (fixture, 0.00)
          val () = B.Fixture.set_friction (fixture, 0.5)
      in body end

  val dudebody = create_dude (BDDMath.vec2 (~5.0, 0.0)) (BDDMath.vec2 (0.0, 0.0)) 0.3
  val Dude (dudeboosters, dudedir) = B.Body.get_data dudebody


  fun create_text_body (text : string)
                       (p : BDDMath.vec2)
                       (v : BDDMath.vec2)
                       (mass : real) : unit = 
      let val pixel_width = (Font.Normal.width - Font.Normal.overlap)
                            * String.size text 
          val pixel_height = Font.Normal.height * 3 div 4
          val meter_width = (Real.fromInt pixel_width) /
                            (Real.fromInt pixelsPerMeter)
          val meter_height = (Real.fromInt pixel_height) /
                             (Real.fromInt pixelsPerMeter)
          val body = B.World.create_body
                         (world,
                          {typ = B.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = v,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = true,
                           bullet = false,
                           active = true,
                           data = Text {text = text,
                                        width = pixel_width,
                                        height = pixel_height},
                           inertia_scale = 1.0
                         })
          val density = mass / meter_width * meter_height
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  meter_height / 2.0)),
                             (),
                             density)
          val () = B.Fixture.set_restitution (fixture, 1.0)
          val () = B.Fixture.set_friction (fixture, 0.0)
      in () end

  val zero = BDDMath.vec2 (0.0, 0.0) 

  fun create_wall (p : BDDMath.vec2)
                  (meter_height : real) : unit = 
      let 
          val pixel_height =
                 Real.round (meter_height * (Real.fromInt pixelsPerMeter))
          val body = B.World.create_body
                         (world,
                          {typ = B.Body.Static,
                           position = p,
                           angle = 0.0,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = true,
                           bullet = false,
                           active = true,
                           data = VerticalLine pixel_height,
                           inertia_scale = 1.0
                         })

          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (0.2,
                                                  meter_height / 2.0)),
                             (),
                             10000.0)
          val () = B.Fixture.set_restitution (fixture, 0.2)
          val () = B.Fixture.set_friction (fixture, 0.0)
      in () end


  fun create_ceiling (p : BDDMath.vec2)
                     (meter_width : real) : unit = 
      let 
          val pixel_width =
                 Real.round (meter_width * (Real.fromInt pixelsPerMeter))
          val body = B.World.create_body
                         (world,
                          {typ = B.Body.Static,
                           position = p,
                           angle = 0.0,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = true,
                           bullet = false,
                           active = true,
                           data = HorizontalLine pixel_width,
                           inertia_scale = 1.0
                         })

          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  0.2)),
                             (),
                             10000.0)
          val () = B.Fixture.set_restitution (fixture, 0.0)
          val () = B.Fixture.set_friction (fixture, 0.2)
      in () end


  val mt =
      MersenneTwister.initstring (Time.toString (Time.now ()))

  fun random_vector max_mag = 
      let open MersenneTwister
          val theta = Math.pi * 2.0 *
               (Real.fromInt (random_nat mt 1000)) / 1000.0
          val mag = max_mag * 
               (Real.fromInt (random_nat mt 1000)) / 1000.0
          val x = mag * (Math.cos theta)
          val y = mag * (Math.sin theta)
      in BDDMath.vec2 (x, y) end

  val () = Util.for 0 2 (fn y =>
               create_text_body "hydrogen" 
                                (random_vector 9.0)
                                (random_vector 5.0)
                                0.1
                         )


  val () = create_text_body "helium"
                            (random_vector 9.0)
                            (random_vector 6.0)
                            0.4

  val () = create_text_body "krypton"
                            (random_vector 9.0)
                            (random_vector 6.0)
                            8.38

  val () = create_text_body "xenon"
                            (random_vector 9.0)
                            (random_vector 6.0)
                            13.12



  val () = create_wall (BDDMath.vec2 (~15.0, 0.0)) 24.0

  val () = create_wall (BDDMath.vec2 (15.0, 0.0)) 24.0

  val () = create_ceiling (BDDMath.vec2 (0.0, 12.0)) 30.0

  val () = create_ceiling (BDDMath.vec2 (0.0, ~12.0)) 30.0


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
      ((#right rpboosters) := true; SOME ControlRoboPlatform)
    | keyDown (SDL.SDLK_LEFT) ControlRoboPlatform =
      ((#left rpboosters) := true; SOME ControlRoboPlatform)
    | keyDown (SDL.SDLK_DOWN)  ControlRoboPlatform = 
      ((#bottom rpboosters) := true; SOME ControlRoboPlatform)

    | keyDown (SDL.SDLK_RIGHT)  ControlDude =
      ((#right dudeboosters) := true;
       dudedir := Right;
       SOME ControlDude)
    | keyDown (SDL.SDLK_LEFT) ControlDude =
      ((#left dudeboosters) := true;
       dudedir := Left;
       SOME ControlDude)

    | keyDown (SDL.SDLK_SPACE) ControlDude = 
      (B.Body.apply_linear_impulse (dudebody,
                                    BDDMath.vec2 (0.0, 4.0),
                                    zero);
       SOME ControlDude)

    | keyDown (SDL.SDLK_f) ControlRoboPlatform = SOME ControlDude
    | keyDown (SDL.SDLK_f) ControlDude = SOME ControlRoboPlatform

    | keyDown _ s = SOME s

  fun keyUp (SDL.SDLK_RIGHT) ControlRoboPlatform =
      ((#right rpboosters) := false; SOME ControlRoboPlatform)
    | keyUp (SDL.SDLK_LEFT)  ControlRoboPlatform =
      ((#left rpboosters) := false; SOME ControlRoboPlatform)
    | keyUp (SDL.SDLK_DOWN)  ControlRoboPlatform = 
      ((#bottom rpboosters) := false; SOME ControlRoboPlatform)

    | keyUp (SDL.SDLK_RIGHT)  ControlDude =
      ((#right dudeboosters) := false; SOME ControlDude)
    | keyUp (SDL.SDLK_LEFT) ControlDude =
      ((#left dudeboosters) := false; SOME ControlDude)

(*    | keyUp (SDL.SDLK_SPACE) ControlDude = 
      ((#bottom dudeboosters) := false; SOME ControlDude) *)

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
