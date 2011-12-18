structure Types = 
struct

  datatype controlmode = ControlDude | ControlRoboPlatform of int
  type state = controlmode 
  type screen = SDL.surface



  type boosters = {bottom : bool ref,
                   left : bool ref,
                   right : bool ref}

  datatype direction = Left | Right




  datatype bodytype = Text of {text : string,
                               width : int,
                               height : int}
                    | VerticalLine of int
                    | HorizontalLine of int
                    | RoboPlatform of boosters
                    | Dude of boosters * (direction ref)

  datatype fixturetype = DudeFixture
                       | RoboPlatformFixture
                       | OtherFixture

  structure B = BDDWorld( 
                struct type fixture_data = int * fixturetype
                       type body_data = bodytype
                       type joint_data = unit
                end
                )



end
