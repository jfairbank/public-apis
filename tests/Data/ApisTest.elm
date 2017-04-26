module Data.ApisTest exposing (all)

import Data.Apis as Apis exposing (Apis, ApiFilters)
import Fixture.Apis exposing (..)
import Expect exposing (Expectation)
import Helper.Expect exposing (expectErr)
import Test exposing (..)


parseApisTests : Test
parseApisTests =
    describe "parseApis"
        [ test "parses out categories" <|
            \() ->
                let
                    ( apiListFixture, valueListFixture ) =
                        apiListByCategoryFixtures [ "Foo", "Bar", "Baz" ]

                    expectedApisFixture : Apis
                    expectedApisFixture =
                        Apis apiListFixture [ "Bar", "Baz", "Foo" ] apiFiltersNothing
                in
                    Apis.parseApis valueListFixture
                        |> Expect.equal (Ok expectedApisFixture)
        , test "empty name fails" <|
            \() ->
                apiListByNameFixtures [ "Fake", "", "Fake" ]
                    |> Tuple.second
                    |> Apis.parseApis
                    |> expectErr "should not parse"
        , test "empty category fails" <|
            \() ->
                apiListByCategoryFixtures [ "Fake", "", "Fake" ]
                    |> Tuple.second
                    |> Apis.parseApis
                    |> expectErr "should not parse"
        , test "empty description fails" <|
            \() ->
                apiListByDescriptionFixtures [ "Fake", "", "Fake" ]
                    |> Tuple.second
                    |> Apis.parseApis
                    |> expectErr "should not parse"
        , test "empty auth fails" <|
            \() ->
                apiListByAuthFixtures [ "Yes", "", "Yes" ]
                    |> Tuple.second
                    |> Apis.parseApis
                    |> expectErr "should not parse"
        , test "empty https fails" <|
            \() ->
                apiListByHttpsFixtures [ "Yes", "", "Yes" ]
                    |> Tuple.second
                    |> Apis.parseApis
                    |> expectErr "should not parse"
        , test "empty link fails" <|
            \() ->
                apiListByLinkFixtures [ "http://example.com", "", "http://foo.com" ]
                    |> Tuple.second
                    |> Apis.parseApis
                    |> expectErr "should not parse"
        , test "invalid urls fail" <|
            \() ->
                apiListByLinkFixtures [ "http://example.com", "badurl", "http://foo.com" ]
                    |> Tuple.second
                    |> Apis.parseApis
                    |> expectErr "should not parse"
        ]


setCategoryFilterTests : Test
setCategoryFilterTests =
    describe "setCategoryFilter"
        [ test "keeps only matching categories" <|
            \() ->
                let
                    apiList =
                        [ apiFromCategory "Foo"
                        , apiFromCategory "Bar"
                        , apiFromCategory "Foo"
                        ]

                    categories =
                        List.map .category apiList
                in
                    Apis apiList categories apiFiltersNothing
                        |> Apis.setCategoryFilter "Foo"
                        |> Apis.filterEntries
                        |> Expect.equal [ apiFromCategory "Foo", apiFromCategory "Foo" ]
        , test "does not filter with empty category" <|
            \() ->
                let
                    apiList =
                        [ apiFromCategory "Foo"
                        , apiFromCategory "Bar"
                        , apiFromCategory "Foo"
                        ]

                    categories =
                        List.map .category apiList
                in
                    Apis apiList categories apiFiltersNothing
                        |> Apis.setCategoryFilter ""
                        |> Apis.filterEntries
                        |> Expect.equal apiList
        ]


setAuthFilterTests : Test
setAuthFilterTests =
    describe "setAuthFilter"
        [ test "keeps only matching auth" <|
            \() ->
                let
                    apiList =
                        [ apiFromAuth "Yes"
                        , apiFromAuth "No"
                        , apiFromAuth "Yes"
                        ]

                    categories =
                        List.map .category apiList
                in
                    Apis apiList categories apiFiltersNothing
                        |> Apis.setAuthFilter "Yes"
                        |> Apis.filterEntries
                        |> Expect.equal [ apiFromAuth "Yes", apiFromAuth "Yes" ]
        , test "does not filter with empty auth" <|
            \() ->
                let
                    apiList =
                        [ apiFromAuth "Yes"
                        , apiFromAuth "No"
                        , apiFromAuth "Yes"
                        ]

                    categories =
                        List.map .category apiList
                in
                    Apis apiList categories apiFiltersNothing
                        |> Apis.setAuthFilter ""
                        |> Apis.filterEntries
                        |> Expect.equal apiList
        ]


all : Test
all =
    describe "Apis"
        [ parseApisTests
        , setCategoryFilterTests
        , setAuthFilterTests
        ]
