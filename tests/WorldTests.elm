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
    """4 8
    (0, 2, N) FFLFRFF
    (2, 3, E) LFRFF
    """


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
                                , rovers =
                                    [ Ok
                                        { x = 0
                                        , y = 2
                                        , orientation = "N"
                                        , instructions = "FFLFRFF"
                                        }
                                    , Ok
                                        { x = 2
                                        , y = 3
                                        , orientation = "E"
                                        , instructions = "LFRFF"
                                        }
                                    ]
                            }
            ]
        ]
