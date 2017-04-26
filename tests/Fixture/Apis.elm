module Fixture.Apis
    exposing
        ( apiFromCategory
        , apiFromAuth
        , apiFiltersNothing
        , apiListByNameFixtures
        , apiListByCategoryFixtures
        , apiListByDescriptionFixtures
        , apiListByAuthFixtures
        , apiListByHttpsFixtures
        , apiListByLinkFixtures
        )

import Data.Apis as Apis exposing (ApiFilters)
import Json.Encode as Encode exposing (Value)
import Data.Api exposing (Api)
import Helper.Op exposing ((=>))
import Helper.Tuple exposing (mapSecond)


apiToValue : Api -> Value
apiToValue api =
    Encode.object
        [ "name" => Encode.string api.name
        , "category" => Encode.string api.category
        , "description" => Encode.string api.description
        , "auth" => Encode.string api.auth
        , "https" => Encode.string api.https
        , "link" => Encode.string api.link
        ]


apiFiltersNothing : ApiFilters
apiFiltersNothing =
    ApiFilters Nothing Nothing Nothing Nothing


categoriesList : List String
categoriesList =
    List.repeat 3 "Fake"


apiFixtures : (a -> Api) -> a -> ( Api, Value )
apiFixtures f value =
    let
        api =
            f value
    in
        ( api, apiToValue api )


apiFromCategory : String -> Api
apiFromCategory category =
    Api "Fake" category "Fake" "Yes" "Yes" "http://example.com"


apiFromAuth : String -> Api
apiFromAuth auth =
    Api "Fake" "Fake" "Fake" auth "Yes" "http://example.com"


apiNameFixtures : String -> ( Api, Value )
apiNameFixtures =
    apiFixtures
        (\name -> Api name "Fake" "Fake" "Yes" "Yes" "http://example.com")


apiCategoryFixtures : String -> ( Api, Value )
apiCategoryFixtures =
    apiFixtures apiFromCategory


apiDescriptionFixtures : String -> ( Api, Value )
apiDescriptionFixtures =
    apiFixtures
        (\description -> Api "Fake" "Fake" description "Yes" "Yes" "http://example.com")


apiAuthFixtures : String -> ( Api, Value )
apiAuthFixtures =
    apiFixtures apiFromAuth


apiHttpsFixtures : String -> ( Api, Value )
apiHttpsFixtures =
    apiFixtures
        (\https -> Api "Fake" "Fake" "Fake" "Yes" https "http://example.com")


apiLinkFixtures : String -> ( Api, Value )
apiLinkFixtures =
    apiFixtures
        (\link -> Api "Fake" "Fake" "Fake" "Yes" "Yes" link)


apiListFixtures : (String -> ( Api, Value )) -> List String -> ( List Api, Value )
apiListFixtures f =
    List.map f
        >> List.unzip
        >> mapSecond Encode.list


apiListByNameFixtures : List String -> ( List Api, Value )
apiListByNameFixtures =
    apiListFixtures apiNameFixtures


apiListByCategoryFixtures : List String -> ( List Api, Value )
apiListByCategoryFixtures =
    apiListFixtures apiCategoryFixtures


apiListByDescriptionFixtures : List String -> ( List Api, Value )
apiListByDescriptionFixtures =
    apiListFixtures apiDescriptionFixtures


apiListByAuthFixtures : List String -> ( List Api, Value )
apiListByAuthFixtures =
    apiListFixtures apiAuthFixtures


apiListByHttpsFixtures : List String -> ( List Api, Value )
apiListByHttpsFixtures =
    apiListFixtures apiHttpsFixtures


apiListByLinkFixtures : List String -> ( List Api, Value )
apiListByLinkFixtures =
    apiListFixtures apiLinkFixtures
