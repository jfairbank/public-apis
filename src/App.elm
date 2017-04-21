module App exposing (..)

import Data.Api exposing (Api)
import Data.Apis exposing (Apis, parseApis)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, href, value)
import Html.Events exposing (onInput)
import Json.Decode exposing (Decoder, Value, map)


type alias Model =
    Result String Apis


type Msg
    = SelectCategory String


init : Value -> ( Model, Cmd Msg )
init apis =
    ( parseApis apis, Cmd.none )


updateCurrentCategoryName : String -> Apis -> Apis
updateCurrentCategoryName categoryName apis =
    { apis | currentCategoryName = categoryName }


update : Msg -> Model -> ( Model, Cmd Msg )
update (SelectCategory name) model =
    ( Result.map (updateCurrentCategoryName name) model
    , Cmd.none
    )


categoryOption : String -> Html Msg
categoryOption name =
    option [ value name ] [ text name ]


emptyOption : Html Msg
emptyOption =
    option [ value "" ] [ text "" ]


categoriesSelect : List String -> Html Msg
categoriesSelect categories =
    div []
        [ label [] [ text "Select API Category:" ]
        , select [ onInput SelectCategory ]
            (emptyOption :: (List.map categoryOption categories))
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
        [ tableCell api.name
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


apiTable : List Api -> Html Msg
apiTable entries =
    table [ class "table table-bordered" ]
        [ tableHeader
            [ columnHeader "API"
            , columnHeader "Description"
            , columnHeader "Auth"
            , columnHeader "HTTPS"
            , columnHeader "Link"
            ]
        , tableBody apiRow entries
        ]


selectedCategory : String -> Dict String (List Api) -> Html Msg
selectedCategory name =
    Dict.get name
        >> Maybe.map apiTable
        >> Maybe.withDefault (text "")


view : Model -> Html Msg
view model =
    case model of
        Ok { categories, byCategory, currentCategoryName } ->
            div [ class "container" ]
                [ div [ class "page-header" ]
                    [ h1 []
                        [ text "Public APIs"
                        ]
                    ]
                , categoriesSelect categories
                , selectedCategory currentCategoryName byCategory
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
