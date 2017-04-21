module Data.Category exposing (..)

import Data.Api exposing (Api, apiDecoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode exposing (Decoder, list, string)


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
