module World exposing
    ( State(..)
    , World
    , init
    , moveRovers
    )

import Parser exposing ((|.), (|=), Parser)



{- Types -}


type alias World =
    { rows : Int
    , columns : Int
    , rovers : List (Result Error Rover)
    }


type alias Rover =
    { x : Int
    , y : Int
    , orientation : String
    , instructions : String
    , state : State
    }


type State
    = Initial
    | Done
    | InBounds
    | Lost


type Error
    = InvalidWorld
    | InvalidRover
    | InvalidInstruction



{- Build -}


init : String -> World
init input =
    case String.lines input of
        world :: rovers ->
            case ( parse world, parseRovers rovers ) of
                ( Ok w, r ) ->
                    { w | rovers = List.map (setState w) r }

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



{- Rovers -}


moveRovers : World -> World
moveRovers world =
    { world
        | rovers =
            List.map (moveRover world) world.rovers
                |> List.reverse
                |> List.map output
    }


output : Result Error Rover -> Result Error Rover
output roverResult =
    case roverResult of
        Ok rover ->
            let
                log =
                    "(" ++ String.fromInt rover.x ++ ", " ++ String.fromInt rover.y ++ ", " ++ rover.orientation ++ ")"

                _ =
                    Debug.log "" <|
                        if rover.state == Lost then
                            log ++ " Lost"

                        else
                            log
            in
            Ok rover

        Err e ->
            Err e


moveRover : World -> Result Error Rover -> Result Error Rover
moveRover ({ rows, columns } as world) roverResult =
    case readInstruction roverResult of
        Ok i ->
            processInstruction rows columns i roverResult
                |> nextInstruction world

        Err e ->
            Err e


readInstruction : Result Error Rover -> Result Error String
readInstruction roverResult =
    case roverResult of
        Ok rover ->
            case String.uncons rover.instructions of
                Just ( 'F', _ ) ->
                    Ok "F"

                Just ( 'L', _ ) ->
                    Ok "L"

                Just ( 'R', _ ) ->
                    Ok "R"

                Nothing ->
                    Ok "Done"

                _ ->
                    Err InvalidInstruction

        Err e ->
            Err e


processInstruction : Int -> Int -> String -> Result Error Rover -> Result Error Rover
processInstruction rows columns instruction roverResult =
    case roverResult of
        Ok rover ->
            if instruction == "F" && canMoveForward rows columns rover then
                Ok <| instructionProcessed <| moveForward rover

            else if instruction == "F" then
                Ok { rover | state = Lost }

            else if instruction == "Done" then
                Ok { rover | state = Done }

            else
                Ok <| instructionProcessed <| rotate instruction rover

        Err e ->
            Err e


nextInstruction : World -> Result Error Rover -> Result Error Rover
nextInstruction world roverResult =
    case roverResult of
        Ok rover ->
            case rover.state of
                InBounds ->
                    moveRover world (Ok rover)

                _ ->
                    Ok rover

        Err e ->
            Err e


instructionProcessed : Rover -> Rover
instructionProcessed rover =
    { rover | instructions = String.dropLeft 1 rover.instructions }


rotate : String -> Rover -> Rover
rotate instruction rover =
    case ( instruction, rover.orientation ) of
        ( "L", "N" ) ->
            orientate "E" rover

        ( "L", "E" ) ->
            orientate "S" rover

        ( "L", "S" ) ->
            orientate "W" rover

        ( "L", "W" ) ->
            orientate "N" rover

        ( "R", "N" ) ->
            orientate "W" rover

        ( "R", "E" ) ->
            orientate "N" rover

        ( "R", "S" ) ->
            orientate "E" rover

        ( "R", "W" ) ->
            orientate "S" rover

        _ ->
            rover


orientate : String -> Rover -> Rover
orientate direction rover =
    { rover | orientation = direction }


moveForward : Rover -> Rover
moveForward rover =
    case rover.orientation of
        "N" ->
            { rover | y = rover.y + 1 }

        "E" ->
            { rover | x = rover.x - 1 }

        "S" ->
            { rover | y = rover.y - 1 }

        "W" ->
            { rover | x = rover.x + 1 }

        _ ->
            rover


setState : World -> Result Error Rover -> Result Error Rover
setState { rows, columns } =
    Result.map
        (\rover ->
            { rover
                | state =
                    if notLost rows columns rover then
                        InBounds

                    else
                        Lost
            }
        )



{- Predicates -}


notLost : Int -> Int -> Rover -> Bool
notLost rows columns { x, y } =
    x >= 0 && x <= columns && y >= 0 && y <= rows


canMoveForward : Int -> Int -> Rover -> Bool
canMoveForward rows columns rover =
    case rover.orientation of
        "N" ->
            notLost rows columns { rover | y = rover.y + 1 }

        "E" ->
            notLost rows columns { rover | x = rover.x - 1 }

        "S" ->
            notLost rows columns { rover | y = rover.y - 1 }

        "W" ->
            notLost rows columns { rover | x = rover.x + 1 }

        _ ->
            False



{- Parsers -}


parse : String -> Result Error World
parse =
    Parser.run worldParser
        >> Result.mapError (\_ -> InvalidWorld)


worldParser : Parser World
worldParser =
    Parser.succeed
        World
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |= Parser.succeed []


parseRovers : List String -> List (Result Error Rover)
parseRovers =
    List.map String.trim
        >> List.filter (not << String.isEmpty)
        >> List.map parseRover


parseRover : String -> Result Error Rover
parseRover =
    Parser.run roverParser
        >> Result.mapError (\_ -> InvalidRover)


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
        |= Parser.succeed Initial
