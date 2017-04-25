module Data.Apis
    exposing
        ( Apis
        , ApiFilters
        , parseApis
        , allLabel
        , setCategoryFilter
        , setAuthFilter
        , setHttpsFilter
        , setSearchFilter
        , filterEntries
        )

import Util.Filter as Filter
import Data.Api exposing (Api)
import Data.Category exposing (Category, categoriesDecoder)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value, decodeValue, map)


type alias FilterF =
    Api -> Bool


type alias ApiFilters =
    { category : Maybe FilterF
    , auth : Maybe FilterF
    , https : Maybe FilterF
    , search : Maybe FilterF
    }


type alias Apis =
    { categories : List String
    , byCategory : Dict String (List Api)
    , all : List Api
    , filters : ApiFilters
    }


extractCategoryNames : List Category -> List String
extractCategoryNames =
    List.map .name


createByCategoryDict : List Category -> Dict String (List Api)
createByCategoryDict =
    List.foldl
        (\category -> Dict.insert category.name category.entries)
        Dict.empty


extractAll : List Category -> List Api
extractAll =
    List.concatMap .entries


apisFromCategories : List Category -> Apis
apisFromCategories categories =
    Apis
        (extractCategoryNames categories)
        (createByCategoryDict categories)
        (extractAll categories)
        (ApiFilters Nothing Nothing Nothing Nothing)


apisDecoder : Decoder Apis
apisDecoder =
    map apisFromCategories categoriesDecoder


parseApis : Value -> Result String Apis
parseApis =
    decodeValue apisDecoder


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
