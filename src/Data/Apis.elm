module Data.Apis exposing (Apis, ApiFilters, parseApis)

import Data.Api exposing (Api)
import Data.Category exposing (Category, categoriesDecoder)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value, decodeValue, map)


type alias FilterF =
    Api -> Bool


type alias ApiFilters =
    { category : Maybe FilterF
    , auth : Maybe FilterF
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
        (ApiFilters Nothing Nothing)


apisDecoder : Decoder Apis
apisDecoder =
    map apisFromCategories categoriesDecoder


parseApis : Value -> Result String Apis
parseApis =
    decodeValue apisDecoder
