module Data.Api exposing (Api, apiDecoder)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode exposing (Decoder, andThen, map, string, succeed, fail)
import Regex exposing (Regex, regex)


type alias Api =
    { name : String
    , category : String
    , description : String
    , auth : String
    , https : String
    , link : String
    }


urlRegex : Regex
urlRegex =
    regex "https?://.*?\\.[a-zA-Z]+"


isUrl : String -> Bool
isUrl =
    Regex.contains urlRegex


validateNotEmpty : String -> Decoder String
validateNotEmpty value =
    if String.trim value == "" then
        fail "Empty value"
    else
        succeed value


validateUrl : String -> Decoder String
validateUrl value =
    if isUrl value then
        succeed value
    else
        fail "Not a URL"


nonEmptyString : Decoder String
nonEmptyString =
    string |> andThen validateNotEmpty


url : Decoder String
url =
    nonEmptyString |> andThen validateUrl


apiDecoder : Decoder Api
apiDecoder =
    decode Api
        |> required "name" nonEmptyString
        |> required "category" nonEmptyString
        |> required "description" nonEmptyString
        |> required "auth" nonEmptyString
        |> required "https" nonEmptyString
        |> required "link" url
