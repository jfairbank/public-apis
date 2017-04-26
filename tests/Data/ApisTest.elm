module Data.ApisTest exposing (all)

import Expect
import Data.Apis as Apis exposing (Apis)
import Fixture.Apis exposing (..)
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


all : Test
all =
    describe "Apis"
        [ parseApisTests
        ]
