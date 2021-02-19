module Main exposing (main)

import Browser
import Html


main : Program String String msg
main =
    Browser.element
        { init = init
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> Html.div [] []
        }


init : String -> ( String, Cmd msg )
init input =
    ( input, Cmd.none )
