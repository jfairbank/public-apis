module Util.Filter exposing (..)


type alias Filter a =
    ( a, Bool )


singleton : a -> Filter a
singleton value =
    ( value, True )


applyMaybe : Maybe (a -> Bool) -> Filter a -> Filter a
applyMaybe maybeF (( value, filtered ) as tuple) =
    case ( maybeF, filtered ) of
        ( Just f, True ) ->
            ( value, f value )

        _ ->
            tuple


extract : Filter a -> Bool
extract =
    Tuple.second
