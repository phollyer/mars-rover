module Main exposing (main)

import Browser
import Html
import World exposing (World)


main : Program String World msg
main =
    Browser.element
        { init = init
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> Html.div [] []
        }


init : String -> ( World, Cmd msg )
init input =
    ( World.init input, Cmd.none )
