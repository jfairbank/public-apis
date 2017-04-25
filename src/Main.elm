module Main exposing (..)

import View exposing (view)
import Update exposing (Model, Msg, update)
import Html exposing (programWithFlags)
import Data.Apis as Apis exposing (Apis)
import Json.Decode exposing (Value)


init : Value -> ( Model, Cmd Msg )
init apis =
    ( Apis.parseApis apis, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Value Model Msg
main =
    programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
