module Data.Apis exposing (Apis, parseApis)

import Data.Api exposing (Api)
import Data.Category exposing (Category, categoriesDecoder)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value, decodeValue, map)


type alias Apis =
    { categories : List String
    , byCategory : Dict String (List Api)
    , currentCategoryName : String
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
        ""


apisDecoder : Decoder Apis
apisDecoder =
    map apisFromCategories categoriesDecoder


parseApis : Value -> Result String Apis
parseApis =
    decodeValue apisDecoder
