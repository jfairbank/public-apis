module Tests exposing (..)

import Data.ApisTest as Apis
import Util.FilterTest as Filter
import Test exposing (..)


all : Test
all =
    describe "public-apis"
        [ Apis.all
        , Filter.all
        ]
