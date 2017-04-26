module Update exposing (Model, Msg(..), update)

import Data.Apis as Apis exposing (Apis)


type alias Model =
    Result String Apis


type Msg
    = SelectCategory String
    | SelectAuth String
    | SelectHttps String
    | Search String


updateApis : Msg -> Apis -> Apis
updateApis msg =
    case msg of
        SelectCategory category ->
            Apis.setCategoryFilter category

        SelectAuth auth ->
            Apis.setAuthFilter auth

        SelectHttps https ->
            Apis.setHttpsFilter https

        Search value ->
            Apis.setSearchFilter value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( Result.map (updateApis msg) model
    , Cmd.none
    )
