module View exposing (view)

import Data.Api exposing (Api)
import Data.Apis as Apis exposing (Apis)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, href, type_, value)
import Html.Events exposing (onInput)
import Update exposing (Model, Msg(..))


categoryOption : String -> Html Msg
categoryOption name =
    option [ value name ] [ text name ]


allOption : Html Msg
allOption =
    option [ value "" ] [ text Apis.allLabel ]


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


httpsSelect : Html Msg
httpsSelect =
    div []
        [ label [] [ text "Filter by HTTPS:" ]
        , select [ onInput SelectHttps ]
            [ allOption
            , option [ value "Yes" ] [ text "Yes" ]
            , option [ value "No" ] [ text "No" ]
            ]
        ]


searchView : Html Msg
searchView =
    div []
        [ label [] [ text "Search:" ]
        , input [ type_ "text", onInput Search ] []
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


apiTableBody : List Api -> Html Msg
apiTableBody entries =
    tbody [] <|
        case entries of
            [] ->
                [ tr []
                    [ td [ class "no-results", colspan 6 ]
                        [ text "No Results" ]
                    ]
                ]

            _ ->
                List.map apiRow entries


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
        , apiTableBody (Apis.filterEntries apis)
        ]


githubRepo : String
githubRepo =
    "https://github.com/jfairbank/public-apis"


filterView : Html Msg -> Html Msg
filterView child =
    div [ class "filter" ] [ child ]


view : Model -> Html Msg
view model =
    case model of
        Ok apis ->
            div [ class "container" ]
                [ div [ class "page-header" ]
                    [ h1 []
                        [ text "Public APIs"
                        , small []
                            [ a [ href githubRepo ] [ text "GitHub Repo" ] ]
                        ]
                    ]
                , div [ class "filters" ]
                    [ filterView (categoriesSelect apis.categories)
                    , filterView authSelect
                    , filterView httpsSelect
                    ]
                , searchView
                , br [] []
                , apiTable apis
                ]

        Err _ ->
            div [ class "container" ]
                [ div [ class "alert alert-danger" ]
                    [ h2 [] [ text "Error parsing APIs" ]
                    ]
                ]
