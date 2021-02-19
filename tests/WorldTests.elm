module WorldTests exposing (..)

import Expect
import Test exposing (Test, describe, test)
import World exposing (..)



{- Helpers -}


world : World
world =
    World.init ""


input : String
input =
    """4 8
    (2, 3, N) FLLFR
    (1, 0, S) FFRLF
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
                                        { x = 2
                                        , y = 3
                                        , orientation = "N"
                                        , instructions = "FLLFR"
                                        , state = InBounds
                                        }
                                    , Ok
                                        { x = 1
                                        , y = 0
                                        , orientation = "S"
                                        , instructions = "FFRLF"
                                        , state = InBounds
                                        }
                                    ]
                            }
            ]
        , describe "running the program"
            [ test "init & run" <|
                \_ ->
                    World.init input
                        |> World.moveRovers
                        |> Expect.equal
                            { world
                                | rows = 4
                                , columns = 8
                                , rovers =
                                    [ Ok
                                        { x = 2
                                        , y = 3
                                        , orientation = "E"
                                        , instructions = ""
                                        , state = Done
                                        }
                                    , Ok
                                        { x = 1
                                        , y = 0
                                        , orientation = "S"
                                        , instructions = "FFRLF"
                                        , state = Lost
                                        }
                                    ]
                            }
            ]
        ]
