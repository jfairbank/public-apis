module App exposing (..)

import Util.Filter as Filter
import Data.Api exposing (Api)
import Data.Apis exposing (Apis, ApiFilters, parseApis)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, href, type_, value)
import Html.Events exposing (onCheck, onInput)
import Json.Decode exposing (Decoder, Value, map)


type alias Model =
    Result String Apis


type Msg
    = SelectCategory String
    | SelectAuth String
    | SelectHttps String
    | Search String


init : Value -> ( Model, Cmd Msg )
init apis =
    ( parseApis apis, Cmd.none )


allLabel : String
allLabel =
    "All"


valueToMaybe : String -> Maybe String
valueToMaybe value =
    if value == "" || value == allLabel then
        Nothing
    else
        Just value


setFilter :
    (String -> Api -> Bool)
    -> (ApiFilters -> Maybe (Api -> Bool) -> ApiFilters)
    -> String
    -> Apis
    -> Apis
setFilter filterf setter value ({ filters } as apis) =
    let
        updatedFilters =
            valueToMaybe value
                |> Maybe.map filterf
                |> setter filters
    in
        { apis | filters = updatedFilters }


categoryFilter : String -> Api -> Bool
categoryFilter value api =
    api.category == value


setCategoryFilter : String -> Apis -> Apis
setCategoryFilter =
    setFilter
        categoryFilter
        (\filters value -> { filters | category = value })


authFilter : String -> Api -> Bool
authFilter value api =
    String.startsWith (String.toLower value) (String.toLower api.auth)


setAuthFilter : String -> Apis -> Apis
setAuthFilter =
    setFilter
        authFilter
        (\filters value -> { filters | auth = value })


httpsFilter : String -> Api -> Bool
httpsFilter value api =
    api.https == value


setHttpsFilter : String -> Apis -> Apis
setHttpsFilter =
    setFilter
        httpsFilter
        (\filters value -> { filters | https = value })


searchFilter : String -> Api -> Bool
searchFilter value api =
    let
        containedIn =
            String.contains (String.toLower value)
    in
        containedIn (String.toLower api.name) || containedIn (String.toLower api.description)


setSearchFilter : String -> Apis -> Apis
setSearchFilter =
    setFilter
        searchFilter
        (\filters value -> { filters | search = value })


applyFilters : ApiFilters -> Api -> Bool
applyFilters filters =
    Filter.singleton
        >> Filter.applyMaybe filters.category
        >> Filter.applyMaybe filters.auth
        >> Filter.applyMaybe filters.https
        >> Filter.applyMaybe filters.search
        >> Filter.extract


filterEntries : Apis -> List Api
filterEntries { filters, all } =
    List.filter (applyFilters filters) all


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

        SelectHttps value ->
            ( Result.map (setHttpsFilter value) model
            , Cmd.none
            )

        Search value ->
            ( Result.map (setSearchFilter value) model
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
        , apiTableBody (filterEntries apis)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
