module App exposing (..)

import Util.Filter as Filter
import Data.Api exposing (Api)
import Data.Apis exposing (Apis, ApiFilters, parseApis)
import Html exposing (..)
import Html.Attributes exposing (class, href, value)
import Html.Events exposing (onInput)
import Json.Decode exposing (Decoder, Value, map)


type alias Model =
    Result String Apis


type Msg
    = SelectCategory String
    | SelectAuth String


init : Value -> ( Model, Cmd Msg )
init apis =
    ( parseApis apis, Cmd.none )


allLabel : String
allLabel =
    "All"


setFilter :
    (Api -> String)
    -> (ApiFilters -> Maybe (Api -> Bool) -> ApiFilters)
    -> String
    -> Apis
    -> Apis
setFilter getField setter value ({ filters } as apis) =
    let
        updatedFilters =
            if value == "" || value == allLabel then
                setter filters Nothing
            else
                setter filters (Just (getField >> (==) value))
    in
        { apis | filters = updatedFilters }


setCategoryFilter : String -> Apis -> Apis
setCategoryFilter =
    setFilter .category
        (\filters value -> { filters | category = value })


setAuthFilter : String -> Apis -> Apis
setAuthFilter =
    setFilter .auth
        (\filters value -> { filters | auth = value })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCategory name ->
            ( Result.map (setCategoryFilter name) model
            , Cmd.none
            )

        SelectAuth name ->
            ( Result.map (setAuthFilter name) model
            , Cmd.none
            )


categoryOption : String -> Html Msg
categoryOption name =
    option [ value name ] [ text name ]


allOption : Html Msg
allOption =
    option [ value "" ] [ text allLabel ]


categoriesSelect : List String -> Html Msg
categoriesSelect categories =
    div []
        [ label [] [ text "Filter by Category:" ]
        , select [ onInput SelectCategory ]
            (allOption :: (List.map categoryOption categories))
        ]


authSelect : Html Msg
authSelect =
    div []
        [ label [] [ text "Filter by Auth:" ]
        , select [ onInput SelectAuth ]
            [ allOption
            , option [ value "Yes" ] [ text "Yes" ]
            , option [ value "No" ] [ text "No" ]
            , option [ value "OAuth" ] [ text "OAuth" ]
            , option [ value "apiKey" ] [ text "apiKey" ]
            , option [ value "X-Mashape-Key" ] [ text "X-Mashape-Key" ]
            , option [ value "token" ] [ text "token" ]
            ]
        ]


columnHeader : String -> Html msg
columnHeader value =
    th [] [ text value ]


tableCell : String -> Html msg
tableCell value =
    td [] [ text value ]


apiRow : Api -> Html Msg
apiRow api =
    tr []
        [ tableCell api.category
        , tableCell api.name
        , tableCell api.description
        , tableCell api.auth
        , tableCell api.https
        , td []
            [ a [ href api.link ] [ text api.link ] ]
        ]


tableHeader : List (Html msg) -> Html msg
tableHeader columnHeaders =
    thead [] [ tr [] columnHeaders ]


tableBody : (a -> Html msg) -> List a -> Html msg
tableBody f items =
    tbody [] (List.map f items)


applyFilters : ApiFilters -> Api -> Bool
applyFilters filters =
    Filter.singleton
        >> Filter.applyMaybe filters.category
        >> Filter.applyMaybe filters.auth
        >> Filter.extract


filterEntries : Apis -> List Api
filterEntries { filters, all } =
    List.filter (applyFilters filters) all


apiTable : Apis -> Html Msg
apiTable apis =
    table [ class "table table-bordered" ]
        [ tableHeader
            [ columnHeader "Category"
            , columnHeader "API"
            , columnHeader "Description"
            , columnHeader "Auth"
            , columnHeader "HTTPS"
            , columnHeader "Link"
            ]
        , tableBody apiRow (filterEntries apis)
        ]


view : Model -> Html Msg
view model =
    case model of
        Ok apis ->
            div [ class "container" ]
                [ div [ class "page-header" ]
                    [ h1 []
                        [ text "Public APIs"
                        ]
                    ]
                , categoriesSelect apis.categories
                , authSelect
                , apiTable apis
                ]

        Err _ ->
            div [ class "container" ]
                [ div [ class "alert alert-danger" ]
                    [ h2 [] [ text "Error parsing APIs" ]
                    ]
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
