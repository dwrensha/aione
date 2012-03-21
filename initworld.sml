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



  val gravity = BDDMath.vec2 (0.0, ~10.0) 
  val world = ref (B.World.world (gravity, true))
  val () = B.World.set_auto_clear_forces (!world,  true)


  local val counter = ref 0
  in fun uniq () = (counter:= !counter + 1; !counter)
  end

  val (exitdoorx, exitdoory) = (ref 0, ref 0)

  val (recording : (int option) ref) = ref NONE
  val (mode : controlmode ref) = ref ControlDude

  val (playback : playbackmode ref) = ref NotPlaying

  val recordingstart = ref (Time.now ())
  val (recordingevents : (Time.time * BoosterEvent) list ref ) = ref nil

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
                  (v : BDDMath.vec2)
                  (mass : real) : B.body = 
      let val pixel_width = 8
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
          val () = B.Fixture.set_friction (fixture, 0.1)
      in body end

  val dudebody =
      ref (create_dude (BDDMath.vec2 (~15.0, 12.0)) (BDDMath.vec2 (0.0, 0.0)) 0.3)



  fun create_roboplatform (i : int)
                          (p : BDDMath.vec2)
                          (v : BDDMath.vec2)
                          (mass : real) : B.body = 
      let val pixel_width = 80
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
          val density = mass / meter_width * meter_height
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
          val density = 1.0 / meter_width * meter_height
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
      (playback := (Playing (Time.now ()));
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
       let val dt = Time.-(Time.now(), !recordingstart)
       in recordingevents :=
          (dt, BottomOff)::(dt, LeftOff)::(dt, RightOff):: (!recordingevents)
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
              in
                  if BDDMath.vec2y d > ~0.5
                  then (* start recording *)
                      (mode := ControlRoboPlatform i;
                       recordingstart := Time.now ();
                       recordingevents := nil;

                       (if !(#right dudeboosters)
                        then recordingevents :=
                              ((Time.zeroTime, LeftOn) :: (!recordingevents))
                        else ());

                       (if !(#left dudeboosters)
                        then recordingevents :=
                             ((Time.zeroTime, RightOn) :: (!recordingevents))
                        else ());


                       copy_flip_boosters
                           (GrowArray.sub rpboosterarray i) dudeboosters;


                       turn_off_boosters dudeboosters
                      )
                  else ()
              end

            | plat_hits_dude i (ControlRoboPlatform j) NotPlaying =
               plat_hits_something i (ControlRoboPlatform j)

            | plat_hits_dude _ _ (Playing _) = ()
              

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
                    (create_dude (BDDMath.vec2 (~15.0, ~13.0))
                                 (BDDMath.vec2 (0.0, 0.0)) 0.3)
               val () = create_wall (BDDMath.vec2 (~18.0, 0.0)) 28.0
               val () = create_wall (BDDMath.vec2 (18.0, 0.0)) 28.0
               val () = create_ceiling (BDDMath.vec2 (0.0, 14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (0.0, ~14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (15.0, 6.0)) 6.0
               val () = create_ceiling (BDDMath.vec2 (~5.0, 6.0)) 26.0
               val (x, y) = worldToScreen (BDDMath.vec2 (15.0, 6.7))
               val () = (exitdoorx := x)
               val () = (exitdoory := y)
               val _ = create_playbutton (BDDMath.vec2 (6.5, ~12.75))
               val () = GrowArray.update rparray 0 
                         (create_roboplatform 0
                            (BDDMath.vec2 (10.0, ~13.25))
                            zero
                            500.0)
               val () = GrowArray.update rpboosterarray 0
                                        let val RoboPlatform bst
                                              = B.Body.get_data
                                                    (GrowArray.sub rparray 0)
                                        in bst end
               val _ = 
                   let open Time
                       val start = fromReal 0.25
                       val cutoff = fromReal 2.25
                       val es = [(start, BottomOn),
                                 (cutoff, BottomOff)]
                   in  GrowArray.update scripts 0 {
                               events = es,
                               remaining = ref nil}
                   end
           in true end
    | 2 => let val () = 
                   dudebody := 
                    (create_dude (BDDMath.vec2 (~15.0, 12.0))
                                 (BDDMath.vec2 (0.0, 0.0)) 0.3)
               val () = create_wall (BDDMath.vec2 (~18.0, 0.0)) 28.0
               val () = create_wall (BDDMath.vec2 (18.0, 0.0)) 28.0
               val () = create_ceiling (BDDMath.vec2 (0.0, 14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (0.0, ~14.0)) 36.0
               val () = create_ceiling (BDDMath.vec2 (15.0, 11.0)) 1.0
               val () = create_ceiling (BDDMath.vec2 (~15.0, 11.0)) 1.0
               val (x, y) = worldToScreen (BDDMath.vec2 (15.0, 11.7))
               val () = (exitdoorx := x)
               val () = (exitdoory := y)
               val _ = create_playbutton (BDDMath.vec2 (~17.0, ~13.0))
               val _ =
                   Util.for 0 4 (fn i => 
                          GrowArray.update rparray i 
                           (create_roboplatform
                            i
                            (BDDMath.vec2 (5.0 * Real.fromInt (i - 2), ~13.25))
                            (BDDMath.vec2 (0.0, 0.0))
                            500.0))
               val _ = 
                   Util.for 0 4 (fn i => 
                       GrowArray.update rpboosterarray i              
                                        let val RoboPlatform bst
                                              = B.Body.get_data
                                                    (GrowArray.sub rparray i)
                                        in bst end)
               val _ = 
                   let open Time
                       val cutoff = fromReal 2.0
                       val es = [(zeroTime, BottomOn),
                                 (cutoff, BottomOff)]
                   in Util.for 0 4 (fn i => 
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
       if setuplevel (level)
       then SOME (level)
       else SOME (~1)
      )

  val _ = (clearworld(); setuplevel 1)



end
