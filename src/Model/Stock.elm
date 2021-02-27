module Model.Stock exposing
    ( AgeRange
    , Stock
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Model.Lifepath as Lifepath exposing (Lifepath)


type alias Stock =
    { lifepaths : List Lifepath
    , ageRanges : List AgeRange
    }


decoder : Decoder Stock
decoder =
    Decode.succeed Stock
        |> required "lifepaths" (Decode.list Lifepath.decoder)
        |> required "ageRanges" (Decode.list ageRangeDecoder)


type alias AgeRange =
    { minAge : Int
    , maxAge : Int
    , physical : Int
    , mental : Int
    }


ageRangeDecoder : Decoder AgeRange
ageRangeDecoder =
    Decode.succeed AgeRange
        |> required "minAge" Decode.int
        |> required "maxAge" Decode.int
        |> required "physical" Decode.int
        |> required "mental" Decode.int
