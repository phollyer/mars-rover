module World exposing
    ( World
    , init
    )

import Parser exposing ((|.), (|=), DeadEnd, Parser)



{- Types -}


type alias World =
    { rows : Int
    , columns : Int
    }



{- Build -}


init : String -> World
init input =
    case String.lines input of
        world :: _ ->
            case parse world of
                Ok w ->
                    w

                Err _ ->
                    { rows = 0
                    , columns = 0
                    }

        _ ->
            { rows = 0
            , columns = 0
            }


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
