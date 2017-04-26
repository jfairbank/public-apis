module Helper.Tuple exposing (mapSecond)


mapSecond : (b -> c) -> ( a, b ) -> ( a, c )
mapSecond f ( a, b ) =
    ( a, f b )
