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



  val gravity = BDDMath.vec2 (0.0, ~15.0) 
  val world = ref (B.World.world (gravity, true))
  val () = B.World.set_auto_clear_forces (!world,  true)


  local val counter = ref 0
  in fun uniq () = (counter:= !counter + 1; !counter)
  end

  val (exitdoorx, exitdoory) = (ref 0, ref 0)

  val (recording : (int option) ref) = ref NONE
  val (mode : controlmode ref) = ref ControlDude

  val (playback : playbackmode ref) = ref NotPlaying

  (* number of ticks since start of recording or start of playing. *)
  val tickcounter = ref 0

  val (recordingevents : (int * BoosterEvent) list ref) = ref nil

  fun new_boosters () = {bottom = ref false,
                         left = ref false,
                         right = ref false}

  fun turn_off_boosters {bottom, left, right} =
      (bottom := false;
       left := false;
       right := false)

  fun copy_flip_boosters {bottom, left, right} (bst2: boosters) =
      (bottom := !(#bottom bst2);
       left := !(#right bst2);
       right := !(#left bst2))

  val zero = BDDMath.vec2 (0.0, 0.0) 


  fun create_dude (p : BDDMath.vec2)
                  (v : BDDMath.vec2) : B.body = 
      let val mass = 0.1
          val pixel_width = 8
          val pixel_height = 26
          val meter_width = (Real.fromInt pixel_width) /
                            (Real.fromInt pixelsPerMeter)
          val meter_height = (Real.fromInt pixel_height) /
                             (Real.fromInt pixelsPerMeter)
          val body = B.World.create_body
                         (!world,
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
                           bullet = true,
                           active = true,
                           data = Dude (new_boosters(), ref Right),
                           inertia_scale = 1.0
                         })
          val density = mass / (meter_width * meter_height)
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  meter_height / 2.0)),
                             (uniq(), DudeFixture),
                             density)
          val () = B.Fixture.set_restitution (fixture, 0.00)
          val () = B.Fixture.set_friction (fixture, 0.7)
      in body end

  val dudebody =
      ref (create_dude (BDDMath.vec2 (~15.0, 12.0)) (BDDMath.vec2 (0.0, 0.0)))



  fun create_roboplatform (i : int)
                          (p : BDDMath.vec2)
                          (v : BDDMath.vec2) : B.body = 
      let val mass = 1500.0
          val pixel_width = 80
          val pixel_height = 16
          val meter_width = (Real.fromInt pixel_width) /
                            (Real.fromInt pixelsPerMeter)
          val meter_height = (Real.fromInt pixel_height) /
                             (Real.fromInt pixelsPerMeter)
          val body = B.World.create_body
                         (!world,
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
          val density = mass / (meter_width * meter_height)
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  meter_height / 2.0)),
                             (uniq (), RoboPlatformFixture i),
                             density)
(*          val rightfixture = B.Body.create_fixture_default
                              (body,
                               BDDShape.Polygon
                                   (BDDPolygon.rotated_box
                                        (0.05,
                                         meter_height / 1.9,
                                         BDDMath.vec2 (meter_width / 2.0, 0.0),
                                         0.0)),
                               (uniq (), RoboPlatformFixture i),
                               0.0)
          val leftfixture = B.Body.create_fixture_default
                              (body,
                               BDDShape.Polygon
                                   (BDDPolygon.rotated_box
                                        (0.05,
                                         meter_height / 1.9,
                                         BDDMath.vec2 (~ meter_width / 2.0, 0.0),
                                         0.0)),
                               (uniq (), RoboPlatformFixture i),
                               0.0) *)
          val () = B.Fixture.set_restitution (fixture, 0.05)
          val () = B.Fixture.set_friction (fixture, 0.5)
      in body end

  (* We need a way to have multiple levels.  *)

  val rparray = GrowArray.empty ()
  val rpboosterarray = GrowArray.empty ()

  val (scripts : scriptstate GrowArray.growarray) = GrowArray.empty ()



  fun create_playbutton
          (p : BDDMath.vec2) : B.body = 
      let val pixel_width = 16
          val pixel_height = 16
          val meter_width = (Real.fromInt pixel_width) /
                            (Real.fromInt pixelsPerMeter)
          val meter_height = (Real.fromInt pixel_height) /
                             (Real.fromInt pixelsPerMeter)
          val body = B.World.create_body
                         (!world,
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
                           data = PlayButton,
                           inertia_scale = 1.0
                         })
          val density = 1.0 / (meter_width * meter_height)
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  meter_height / 2.0)),
                             (uniq (), PlayButtonFixture),
                             density)
          val () = B.Fixture.set_restitution (fixture, 0.05)
          val () = B.Fixture.set_friction (fixture, 0.5)
      in body end



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
                         (!world,
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
          val density = mass / (meter_width * meter_height)
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  meter_height / 2.0)),
                             (uniq (), OtherFixture),
                             density)
          val () = B.Fixture.set_restitution (fixture, 0.2)
          val () = B.Fixture.set_friction (fixture, 0.4)
      in () end


  fun create_wall (p : BDDMath.vec2)
                  (meter_height : real) : unit = 
      let 
          val pixel_height =
                 Real.round (meter_height * (Real.fromInt pixelsPerMeter))
          val body = B.World.create_body
                         (!world,
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
                                 (BDDPolygon.box (0.05,
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
                         (!world,
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
                                                  0.05)),
                             (uniq (), OtherFixture),
                             10000.0)
          val () = B.Fixture.set_restitution (fixture, 0.0)
          val () = B.Fixture.set_friction (fixture, 0.4)
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



  fun startplaying () =
      (playback := Playing;
       tickcounter := 0;
       Util.for 0 ((GrowArray.length rparray) - 1) (fn i =>
           let val rp = GrowArray.sub rparray i
               val rpboosters = GrowArray.sub rpboosterarray i
               val {bottom, left, right} = rpboosters
               val {events, remaining} = GrowArray.sub scripts i
           in remaining := events
           end)
      )

  fun stoprecording i = 
      let val Dude (dudeboosters, dudedir) = B.Body.get_data (!dudebody)
      in
      (mode := ControlDude;
       let val tc = !tickcounter
       in recordingevents :=
          (tc, BottomOff)::(tc, LeftOff)::(tc, RightOff):: (!recordingevents)
       end;
       GrowArray.update scripts i {events = List.rev (!recordingevents),
                                   remaining = ref nil};
       
       copy_flip_boosters dudeboosters
                          (GrowArray.sub rpboosterarray i);
       (if !(#left dudeboosters) andalso
           not (!(#right dudeboosters))
        then dudedir := Left
        else ());
       (if !(#right dudeboosters) andalso
           not (!(#left dudeboosters))
        then dudedir := Right
        else ());
       (#bottom dudeboosters) := false;
       turn_off_boosters (GrowArray.sub rpboosterarray i)
      )
      end

(* If dude and roboplat collide, start controlling the roboplat.
   If roboplat collides with anything else, stop recording. *)
  fun contact_listener c =
      let open B.Contact
          val (fa, fb) = get_fixtures c
          val (ida, tpa) = B.Fixture.get_data fa
          val (idb, tpb) = B.Fixture.get_data fb
          val Dude (dudeboosters, dudedir) = B.Body.get_data (!dudebody)

          fun plat_hits_something i ControlDude = ()
            | plat_hits_something i (ControlRoboPlatform j) = 
              if i = j
              then stoprecording i
              else ()

          fun plat_hits_dude i ControlDude NotPlaying = 
              let val dp = B.Body.get_position (!dudebody)
                  val pp = B.Body.get_position (GrowArray.sub rparray i)
                  val d = BDDMath.vec2sub (pp, dp)
                  val v = BDDMath.vec2length
                              (B.Body.get_linear_velocity 
                                (GrowArray.sub rparray i))
              in 
                  (* Only start recording if the dude
                     is low enough relative to the platform
                     and the platform's velocity is small enough. *)
                  if BDDMath.vec2y d > ~0.75
                     andalso v < 2.0
                  then (* start recording *)
                      (mode := ControlRoboPlatform i;
                       tickcounter := 0;
                       recordingevents := nil;

                       (if !(#right dudeboosters)
                        then recordingevents :=
                              ((0, LeftOn) :: (!recordingevents))
                        else ());

                       (if !(#left dudeboosters)
                        then recordingevents :=
                             ((0, RightOn) :: (!recordingevents))
                        else ());

                       copy_flip_boosters
                           (GrowArray.sub rpboosterarray i) dudeboosters;

                       turn_off_boosters dudeboosters
                      )
                  else ()
              end

            | plat_hits_dude i (ControlRoboPlatform j) NotPlaying =
              ()
               (* plat_hits_something i (ControlRoboPlatform j) *)

            | plat_hits_dude _ _ Playing = ()
              

            fun dude_hits_play ControlDude NotPlaying =
                (startplaying ()
                )
              | dude_hits_play _ _ = ()
              
              
      in case (tpa, tpb) of
             (DudeFixture, RoboPlatformFixture i) => 
                plat_hits_dude i (!mode) (!playback)
           | (RoboPlatformFixture i, DudeFixture) => 
                plat_hits_dude i (!mode) (!playback)
           | (RoboPlatformFixture i, RoboPlatformFixture j) => 
                (plat_hits_something i (!mode);
                 plat_hits_something j (!mode))
           | (RoboPlatformFixture i, _) => 
                plat_hits_something i (!mode)
           | (_, RoboPlatformFixture i) => 
                plat_hits_something i (!mode)
           | (PlayButtonFixture, DudeFixture) => 
                dude_hits_play (!mode) (!playback)
           | (DudeFixture, PlayButtonFixture) => 
                dude_hits_play (!mode) (!playback) 
           | _ => ()

      end 



  fun setuplevel i =
    (case i of
      1 => let val () = 
                   dudebody := 
                    (create_dude (BDDMath.vec2 (~15.0, ~13.4))
                                 (BDDMath.vec2 (0.0, 0.0)))
               val () = create_wall (BDDMath.vec2 (~18.0, 0.0)) 28.0
               val () = create_wall (BDDMath.vec2 (18.0, 0.0)) 28.0
               val () = create_ceiling (BDDMath.vec2 (0.0, 14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (0.0, ~14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (15.0, 6.0)) 6.0
               val () = create_ceiling (BDDMath.vec2 (~5.0, 6.0)) 26.0
               val (x, y) = worldToScreen (BDDMath.vec2 (15.0, 6.7))
               val () = (exitdoorx := x)
               val () = (exitdoory := y)
               val _ = create_playbutton (BDDMath.vec2 (~7.25, ~13.5))
               val _ = create_playbutton (BDDMath.vec2 (7.25, ~13.5))

               val () = create_text_body
                        "An Extensible Platform for Upwards and Sidewards Mobility"
                         (BDDMath.vec2 (~5.0, 9.0))
                         zero
                         10.0
               val () = create_text_body
                        "by David Renshaw"
                         (BDDMath.vec2 (~7.0, 7.0))
                         zero
                         10.0
               val () = GrowArray.update rparray 0 
                         (create_roboplatform 0
                            (BDDMath.vec2 (10.0, ~13.5))
                            zero)
               val () = GrowArray.update rparray 1
                         (create_roboplatform 1
                            (BDDMath.vec2 (16.0, ~13.5))
                            zero)
               val () = GrowArray.update rparray 2
                         (create_roboplatform 2
                            (BDDMath.vec2 (16.0, ~12.5))
                            zero)
               val () = Util.for 0 2 
                                 (fn i => GrowArray.update rpboosterarray i
                                        let val RoboPlatform bst
                                              = B.Body.get_data
                                                    (GrowArray.sub rparray i)
                                        in bst end
                                 )

               val _ = 
                   let open Time
                       val start = 25
                       val cutoff = 220
                       val es = [(start, BottomOn),
                                 (cutoff, BottomOff)]
                   in  GrowArray.update scripts 0 {
                               events = es,
                               remaining = ref nil} ;
                       GrowArray.update scripts 1 {
                               events = nil,
                               remaining = ref nil} ;
                       GrowArray.update scripts 2 {
                               events = nil,
                               remaining = ref nil} 
                   end
           in true end
    | 2 => let val () = 
                   dudebody := 
                    (create_dude (BDDMath.vec2 (~13.0, 12.6))
                                 (BDDMath.vec2 (0.0, 0.0)))
               val () = create_wall (BDDMath.vec2 (~18.0, 0.0)) 28.0
               val () = create_wall (BDDMath.vec2 (18.0, 0.0)) 28.0
               val () = create_ceiling (BDDMath.vec2 (0.0, 14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (0.0, ~14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (13.0, 12.0)) 1.0
               val () = create_ceiling (BDDMath.vec2 (~13.0, 12.0)) 1.0
               val (x, y) = worldToScreen (BDDMath.vec2 (13.0, 12.7))
               val () = (exitdoorx := x)
               val () = (exitdoory := y)
               val _ = create_playbutton (BDDMath.vec2 (~12.75, ~13.15))
               val () = create_ceiling (BDDMath.vec2 (~15.5, ~12.5)) 5.0
(*               val () = create_wall (BDDMath.vec2 (13.0, ~13.3)) 1.0
               val () = create_ceiling (BDDMath.vec2 (15.5, ~12.8)) 5.0
*)
               val _ =
                   Util.for 0 4 (fn i => 
                          GrowArray.update rparray i 
                           (create_roboplatform
                            i
                            (BDDMath.vec2 (5.0 * Real.fromInt (i - 2), ~13.5))
                            (BDDMath.vec2 (0.0, 0.0))
                                ))
               val _ = 
                   Util.for 0 4 (fn i => 
                       GrowArray.update rpboosterarray i              
                                        let val RoboPlatform bst
                                              = B.Body.get_data
                                                    (GrowArray.sub rparray i)
                                        in bst end)
               val _ = 
                   let open Time
                       val cutoff = 275
                       val es = [(0, BottomOn),
                                 (cutoff, BottomOff)]
                   in Util.for 0 4 (fn i => 
                           GrowArray.update scripts i {
                               events = es,
                               remaining = ref nil})
                   end
           in true end
    | 3 => let val () = 
                   dudebody := 
                    (create_dude (BDDMath.vec2 (~15.0, ~13.6))
                                 (BDDMath.vec2 (0.0, 0.0)))
               val () = create_wall (BDDMath.vec2 (~18.0, 0.0)) 28.0
               val () = create_wall (BDDMath.vec2 (18.0, 0.0)) 28.0
               val () = create_ceiling (BDDMath.vec2 (0.0, 14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (0.0, ~14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (13.0, 12.0)) 1.0
               val (x, y) = worldToScreen (BDDMath.vec2 (13.0, 12.7))
               val () = (exitdoorx := x)
               val () = (exitdoory := y)
               val _ = create_playbutton (BDDMath.vec2 (~17.25, ~13.25))


               val _ =
                   Util.for 0 2 (fn i => 
                          GrowArray.update rparray i 
                           (create_roboplatform
                            i
                            (BDDMath.vec2 (5.0 * Real.fromInt (( i) - 2), ~13.5))
                            (BDDMath.vec2 (0.0, 0.0))
                                ))
               val _ = 
                   Util.for 0 2 (fn i => 
                       GrowArray.update rpboosterarray i              
                                        let val RoboPlatform bst
                                              = B.Body.get_data
                                                    (GrowArray.sub rparray i)
                                        in bst end)
               val _ = 
                   let open Time
                       val cutoff = 275
                       val es = [(0, BottomOn),
                                 (cutoff, BottomOff)]
                   in Util.for 0 2 (fn i => 
                           GrowArray.update scripts i {
                               events = es,
                               remaining = ref nil})
                   end
           in true end
    | 4 => let val () = 
                   dudebody := 
                    (create_dude (BDDMath.vec2 (~15.0, ~7.4))
                                 (BDDMath.vec2 (0.0, 0.0)))
               val () = create_wall (BDDMath.vec2 (~18.0, 0.0)) 28.0
               val () = create_wall (BDDMath.vec2 (18.0, 0.0)) 28.0
               val () = create_ceiling (BDDMath.vec2 (0.0, 14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (0.0, ~14.0)) 36.0

               val () = create_ceiling (BDDMath.vec2 (11.0, 0.0)) 14.0 
               val () = create_ceiling (BDDMath.vec2 (~9.0, 0.0)) 18.0


               val () = create_wall (BDDMath.vec2 (0.0, 4.0)) 8.0
(*               val () = create_wall (BDDMath.vec2 (~2.0, ~0.5)) 1.0 *)

               val () = create_ceiling (BDDMath.vec2 (~8.0, ~8.0)) 20.0

               (* make a staircase *)
               val () = create_wall (BDDMath.vec2 (2.0, ~8.5)) 1.0
               val () = create_ceiling (BDDMath.vec2 (3.0, ~9.0)) 2.0
               val () = create_wall (BDDMath.vec2 (4.0, ~9.5)) 1.0
               val () = create_ceiling (BDDMath.vec2 (5.0, ~10.0)) 2.0
               val () = create_wall (BDDMath.vec2 (6.0, ~10.5)) 1.0
               val () = create_ceiling (BDDMath.vec2 (7.0, ~11.0)) 2.0
               val () = create_wall (BDDMath.vec2 (8.0, ~11.5)) 1.0
               val () = create_ceiling (BDDMath.vec2 (9.0, ~12.0)) 2.0
               val () = create_wall (BDDMath.vec2 (10.0, ~12.5)) 1.0
               val () = create_ceiling (BDDMath.vec2 (11.0, ~13.0)) 2.0
               val () = create_wall (BDDMath.vec2 (12.0, ~13.5)) 1.0


               val (x, y) = worldToScreen (BDDMath.vec2 (~15.0, 0.7))
               val () = (exitdoorx := x)
               val () = (exitdoory := y)

               val _ = create_playbutton (BDDMath.vec2 (~10.0, ~7.5))

               val () = GrowArray.update rparray 0 
                         (create_roboplatform 0
                            (BDDMath.vec2 (~1.0, ~7.5))
                            zero)
               val () = GrowArray.update rpboosterarray 0
                                        let val RoboPlatform bst
                                              = B.Body.get_data
                                                    (GrowArray.sub rparray 0)
                                        in bst end
               val _ = 
                   let open Time
                       val start = 200
                       val cutoff = 400
                       val es = [(start, BottomOn),
                                 (cutoff, BottomOff)]
                   in  GrowArray.update scripts 0 {
                               events = es,
                               remaining = ref nil}
                   end
           in true end
    | 5 => let val () = 
                   dudebody := 
                    (create_dude (BDDMath.vec2 (~15.0, ~13.6))
                                 (BDDMath.vec2 (0.0, 0.0)))
               val () = create_wall (BDDMath.vec2 (~18.0, 0.0)) 28.0
               val () = create_wall (BDDMath.vec2 (18.0, 0.0)) 28.0
               val () = create_ceiling (BDDMath.vec2 (0.0, 14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (0.0, ~14.0)) 36.0

               val () = create_text_body
                        "You win!"
                         (BDDMath.vec2 (15.0, 7.0))
                         zero
                         10.0       

               val (x, y) = worldToScreen (BDDMath.vec2 (100.0, 100.0))
               val () = (exitdoorx := x)
               val () = (exitdoory := y)

               val _ = create_playbutton (BDDMath.vec2 (~17.25, ~13.25))

               val num_plats = 100
               val _ =
                   Util.for 0 (num_plats - 1) (fn i => 
                          GrowArray.update rparray i 
                           (create_roboplatform
                            i
                            (BDDMath.vec2 (5.0 * Real.fromInt (( Int.mod(i,5) - 2)),
                                           ~13.5 + Real.fromInt (i div 5)))
                            (BDDMath.vec2 (0.0, 0.0))
                                ))
               val _ = 
                   Util.for 0 (num_plats - 1) (fn i => 
                       GrowArray.update rpboosterarray i              
                                        let val RoboPlatform bst
                                              = B.Body.get_data
                                                    (GrowArray.sub rparray i)
                                        in bst end)
               val _ = 
                   let open Time
                       val cutoff = 275
                       val es = [(0, BottomOn),
                                 (cutoff, BottomOff)]
                   in Util.for 0 (Int.-(num_plats, 1)) (fn i => 
                           GrowArray.update scripts i {
                               events = es,
                               remaining = ref nil})
                   end
           in true end


    | _ => false
    )

  fun clearworld () = 
     ( (world := (B.World.world (gravity, true)));
       B.World.set_begin_contact (!world, contact_listener)
     )

  fun gotolevel level =
      (clearworld();
       GrowArray.clear rparray;
       GrowArray.clear rpboosterarray;
       GrowArray.clear scripts;
                  
       tickcounter := 0;
       playback := NotPlaying;
       mode := ControlDude;
       if setuplevel (level)
       then SOME (level)
       else SOME (~1)
      )

  val _ = (clearworld(); setuplevel 1)



end
