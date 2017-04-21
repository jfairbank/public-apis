module Main exposing (..)

import App exposing (..)
import Html exposing (programWithFlags)
import Json.Decode exposing (Value)


main : Program Value Model Msg
main =
    programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
