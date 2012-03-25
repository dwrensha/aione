structure Types = 
struct


  type state = int (* room number *)
  type screen = SDL.surface



  type boosters = {bottom : bool ref,
                   left : bool ref,
                   right : bool ref}

  datatype direction = Left | Right


  datatype BoosterEvent = BottomOn
                        | BottomOff
                        | LeftOn
                        | LeftOff
                        | RightOn
                        | RightOff

  type scriptstate = {events : (int * BoosterEvent) list,
                      remaining : (int * BoosterEvent) list ref}


  datatype playbackmode = NotPlaying | Playing

 
 (* what else might exist? broken roboplatforms. platforms that follow targets. *)

  datatype bodytype = Text of {text : string,
                               width : int,
                               height : int}
                    | VerticalLine of int
                    | HorizontalLine of int
                    | RoboPlatform of boosters
                    | PlayButton
                    | Dude of boosters * (direction ref)

  datatype fixturetype = DudeFixture
                       | RoboPlatformFixture of int (* index into array *)
                       | PlayButtonFixture
                       | OtherFixture

  datatype controlmode = ControlDude | ControlRoboPlatform of int

  structure B = BDDWorld(
                            (* id number and data *)
                struct type fixture_data = int * fixturetype
                       type body_data = bodytype
                       type joint_data = unit
                end
                )




end
