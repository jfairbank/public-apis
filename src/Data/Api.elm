module Data.Api exposing (..)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode exposing (Decoder, string)


type alias Api =
    { name : String
    , category : String
    , description : String
    , auth : String
    , https : String
    , link : String
    }


apiDecoder : Decoder Api
apiDecoder =
    decode Api
        |> required "name" string
        |> required "category" string
        |> required "description" string
        |> required "auth" string
        |> required "https" string
        |> required "link" string
