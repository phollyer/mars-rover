module World exposing
    ( World
    , init
    )

import Parser exposing ((|.), (|=), DeadEnd, Parser)



{- Types -}


type alias World =
    { rows : Int
    , columns : Int
    , rovers : List (Result (List DeadEnd) Rover)
    }


type alias Rover =
    { x : Int
    , y : Int
    , orientation : String
    , instructions : String
    }



{- Build -}


init : String -> World
init input =
    case String.lines input of
        world :: rovers ->
            case ( parse world, parseRovers rovers ) of
                ( Ok w, r ) ->
                    { w | rovers = r }

                _ ->
                    { rows = 0
                    , columns = 0
                    , rovers = []
                    }

        _ ->
            { rows = 0
            , columns = 0
            , rovers = []
            }



{- Parsers -}


parse : String -> Result (List DeadEnd) World
parse =
    Parser.run worldParser


worldParser : Parser World
worldParser =
    Parser.succeed
        World
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |= Parser.succeed []


parseRovers : List String -> List (Result (List DeadEnd) Rover)
parseRovers =
    List.map String.trim
        >> List.filter (not << String.isEmpty)
        >> List.map (Parser.run roverParser)


roverParser : Parser Rover
roverParser =
    Parser.succeed
        Rover
        |. Parser.symbol "("
        |= Parser.int
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol ","
        |. Parser.spaces
        |= (Parser.chompWhile (\char -> List.member char [ 'N', 'S', 'E', 'W' ])
                |> Parser.getChompedString
           )
        |. Parser.symbol ")"
        |. Parser.spaces
        |= (Parser.chompUntilEndOr "$"
                |> Parser.getChompedString
           )
