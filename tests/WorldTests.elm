module WorldTests exposing (..)

import Expect
import Test exposing (Test, describe, test)
import World exposing (..)



{- Helpers -}


world : World
world =
    World.init ""


input1 : String
input1 =
    """4 8
    (2, 3, N) FLLFR
    (1, 0, S) FFRLF
    """


input2 : String
input2 =
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
                    World.init input1
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
            [ test "input1" <|
                \_ ->
                    World.init input1
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
            , test "input2" <|
                \_ ->
                    World.init input2
                        |> World.moveRovers
                        |> Expect.equal
                            { world
                                | rows = 4
                                , columns = 8
                                , rovers =
                                    [ Ok
                                        { x = 0
                                        , y = 4
                                        , orientation = "E"
                                        , instructions = "FRFF"
                                        , state = Lost
                                        }
                                    , Ok
                                        { x = 0
                                        , y = 2
                                        , orientation = "E"
                                        , instructions = ""
                                        , state = Done
                                        }
                                    ]
                            }
            ]
        ]
