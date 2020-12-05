module Common.NonZero exposing
    ( NonZero
    , decode
    , fromInt
    , toInt
    )

import Json.Decode as Decode exposing (Decoder)


type NonZero
    = NonZero Int


toInt : NonZero -> Int
toInt (NonZero val) =
    val


fromInt : Int -> Maybe NonZero
fromInt val =
    if val == 0 then
        Nothing

    else
        Just <| NonZero val


decode : Decoder NonZero
decode =
    Decode.andThen attemptDecode Decode.int


attemptDecode : Int -> Decoder NonZero
attemptDecode val =
    case fromInt val of
        Nothing ->
            Decode.fail "Got zero for a non-zero value"

        Just nz ->
            Decode.succeed nz
