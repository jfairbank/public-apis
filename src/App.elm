module App exposing (..)

import Dict exposing (Dict)
import Html.Attributes exposing (class, href, value)
import Html.Events exposing (onInput)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode
    exposing
        ( Decoder
        , Value
        , decodeValue
        , list
        , map
        , string
        )
import Html
    exposing
        ( Html
        , a
        , br
        , div
        , h1
        , h2
        , label
        , li
        , option
        , small
        , select
        , table
        , tbody
        , td
        , th
        , thead
        , tr
        , text
        , ul
        )


type alias Api =
    { name : String
    , description : String
    , auth : String
    , https : String
    , link : String
    }


apiDecoder : Decoder Api
apiDecoder =
    decode Api
        |> required "name" string
        |> required "description" string
        |> required "auth" string
        |> required "https" string
        |> required "link" string


type alias Category =
    { name : String
    , entries : List Api
    }


categoryDecoder : Decoder Category
categoryDecoder =
    decode Category
        |> required "name" string
        |> required "entries" (list apiDecoder)


categoriesDecoder : Decoder (List Category)
categoriesDecoder =
    list categoryDecoder


type alias Apis =
    { categories : List String
    , byCategory : Dict String (List Api)
    , currentCategoryName : Maybe String
    }


extractCategoryNames : List Category -> List String
extractCategoryNames =
    List.map .name


createByCategoryDict : List Category -> Dict String (List Api)
createByCategoryDict =
    List.foldl
        (\category -> Dict.insert category.name category.entries)
        Dict.empty


apisFromCategories : List Category -> Apis
apisFromCategories categories =
    Apis
        (extractCategoryNames categories)
        (createByCategoryDict categories)
        Nothing


apisDecoder : Decoder Apis
apisDecoder =
    map apisFromCategories categoriesDecoder


type Model
    = ParseError String
    | WithApis Apis


parseApis : Value -> Model
parseApis json =
    case decodeValue apisDecoder json of
        Ok apis ->
            WithApis apis

        Err error ->
            ParseError error


init : Value -> ( Model, Cmd Msg )
init apis =
    ( parseApis apis, Cmd.none )


type Msg
    = SelectCategory String


update : Msg -> Model -> ( Model, Cmd Msg )
update (SelectCategory name) model =
    case model of
        WithApis apis ->
            let
                updatedModel =
                    if name == "" then
                        WithApis { apis | currentCategoryName = Nothing }
                    else
                        WithApis { apis | currentCategoryName = Just name }
            in
                ( updatedModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


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


selectedCategory : Dict String (List Api) -> Maybe String -> Html Msg
selectedCategory byCategory maybeName =
    Maybe.andThen (\name -> Dict.get name byCategory) maybeName
        |> Maybe.map apiTable
        |> Maybe.withDefault (text "")


repoUrl : String
repoUrl =
    "https://github.com/toddmotto/public-apis"


view : Model -> Html Msg
view model =
    case model of
        WithApis { categories, byCategory, currentCategoryName } ->
            div [ class "container" ]
                [ div [ class "page-header" ]
                    [ h1 []
                        [ text "Public APIs"
                        , br [] []
                        , small []
                            [ a [ href repoUrl ] [ text repoUrl ] ]
                        ]
                    ]
                , categoriesSelect categories
                , selectedCategory byCategory currentCategoryName
                ]

        ParseError _ ->
            div [ class "container" ]
                [ div [ class "alert alert-danger" ]
                    [ h2 [] [ text "Error parsing APIs" ]
                    ]
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
