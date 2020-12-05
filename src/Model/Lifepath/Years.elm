module Model.Lifepath.Years exposing (Years(..), decoder, toString)

import Common
import Json.Decode as Decode exposing (Decoder)


type Years
    = Count Int
    | Range ( Int, Int )


toString : Years -> String
toString years =
    case years of
        Count y ->
            String.fromInt y ++ " yrs"

        Range ( min, max ) ->
            String.fromInt min ++ "-" ++ String.fromInt max ++ " yrs"


decoder : Decoder Years
decoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen yearKindsDecoder


yearKindsDecoder : String -> Decoder Years
yearKindsDecoder kind =
    case kind of
        "count" ->
            yearCountDecoder

        "range" ->
            yearRangeDecoder

        k ->
            Decode.fail <| "Invalid year kind: " ++ k


yearCountDecoder : Decoder Years
yearCountDecoder =
    Decode.map Count (Decode.field "value" Decode.int)


yearRangeDecoder : Decoder Years
yearRangeDecoder =
    Decode.map Range (Common.pairDecoder Decode.int Decode.int)
