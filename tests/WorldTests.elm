module WorldTests exposing (..)

import Expect
import Test exposing (Test, describe, test)
import World exposing (World)



{- Helpers -}


world : World
world =
    World.init ""


input : String
input =
    "4 8"


suite : Test
suite =
    describe "The World module"
        [ describe "init"
            [ test "initilizing the world successfully" <|
                \_ ->
                    World.init input
                        |> Expect.equal
                            { world
                                | rows = 4
                                , columns = 8
                            }
            ]
        ]
