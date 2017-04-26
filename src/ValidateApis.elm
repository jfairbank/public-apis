port module ValidateApis exposing (..)

import Data.Apis exposing (Apis, parseApis)
import Json.Encode exposing (Value)
import Platform exposing (program)


port receiveApis : (Value -> msg) -> Sub msg


port isValid : Bool -> Cmd msg


type alias Model =
    ()


type Msg
    = ReceiveApis Value


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


validate : Result String Apis -> Bool
validate =
    Result.map (always True)
        >> Result.withDefault False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveApis value ->
            ( model
            , isValid (validate (parseApis value))
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveApis ReceiveApis


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
