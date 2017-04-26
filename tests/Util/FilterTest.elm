module Util.FilterTest exposing (..)

import Expect
import Util.Filter as Filter
import Test exposing (..)


all : Test
all =
    describe "Filter"
        [ test "Applies filter to return True" <|
            \() ->
                Filter.singleton 42
                    |> Filter.applyMaybe (Just (\n -> n > 41))
                    |> Filter.extract
                    |> Expect.true "should have True"
        , test "Applies filter to return False" <|
            \() ->
                Filter.singleton 42
                    |> Filter.applyMaybe (Just (\n -> n < 41))
                    |> Filter.extract
                    |> Expect.false "should have False"
        , test "Multiple filters return True" <|
            \() ->
                Filter.singleton 42
                    |> Filter.applyMaybe (Just (\n -> n > 41))
                    |> Filter.applyMaybe (Just (\n -> n < 43))
                    |> Filter.extract
                    |> Expect.true "should have True"
        , test "Multiple filters return False" <|
            \() ->
                Filter.singleton 42
                    |> Filter.applyMaybe (Just (\n -> n < 41))
                    |> Filter.applyMaybe (Just (\n -> n > 43))
                    |> Filter.extract
                    |> Expect.false "should have False"
        , test "Multiple filters return False even if one True" <|
            \() ->
                Filter.singleton 42
                    |> Filter.applyMaybe (Just (\n -> n < 41))
                    |> Filter.applyMaybe (Just (\n -> n < 43))
                    |> Filter.extract
                    |> Expect.false "should have False"
        ]
