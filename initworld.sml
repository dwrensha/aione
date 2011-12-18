structure InitWorld = 
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


  val gravity = BDDMath.vec2 (0.0, ~10.0) 
  val world = B.World.world (gravity, true)
  val () = B.World.set_auto_clear_forces (world,  true)


  local val counter = ref 0
  in fun uniq () = (counter:= !counter + 1; !counter)
  end


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
                           allow_sleep = true,
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
                             (uniq (), RoboPlatformFixture),
                             density)
          val () = B.Fixture.set_restitution (fixture, 0.1)
          val () = B.Fixture.set_friction (fixture, 0.5)
      in body end

  val rparray = Array.tabulate
                (7,
              fn i =>
                 create_roboplatform
                     (BDDMath.vec2 (5.0 * Real.fromInt (i - 3), ~11.0))
                     (BDDMath.vec2 (0.0, 0.0)) 1.0)

  val rp = Array.sub (rparray, 0)
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
                             (uniq(), DudeFixture),
                             density)
          val () = B.Fixture.set_restitution (fixture, 0.00)
          val () = B.Fixture.set_friction (fixture, 0.5)
      in body end

  val dudebody = create_dude (BDDMath.vec2 (~17.0, 11.0)) (BDDMath.vec2 (0.0, 0.0)) 0.3
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
                             (uniq (), OtherFixture),
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
                           allow_sleep = true,
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
                             (uniq (), OtherFixture),
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
                             (uniq (), OtherFixture),
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


  val () = create_wall (BDDMath.vec2 (~18.0, 0.0)) 28.0

  val () = create_wall (BDDMath.vec2 (18.0, 0.0)) 28.0

  val () = create_ceiling (BDDMath.vec2 (0.0, 14.0)) 36.0

  val () = create_ceiling (BDDMath.vec2 (0.0, ~14.0)) 36.0

  val () = create_ceiling (BDDMath.vec2 (17.0, 10.0)) 2.0

  val () = create_ceiling (BDDMath.vec2 (~17.0, 10.0)) 2.0

  fun contact_listener c = ()

(* If dude and roboplat collide, start controlling the roboplat.
   If roboplat collides with anything else, stop recording. *)
  val () = B.World.set_begin_contact (world, contact_listener)
      

end
