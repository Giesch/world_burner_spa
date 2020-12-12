module Model.Lifepath.Years exposing
    ( Years
    , age
    , decoder
    , toString
    )

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


age : List Years -> Int
age list =
    let
        toInt : Years -> Int
        toInt yrs =
            case yrs of
                Count count ->
                    count

                Range ( min, _ ) ->
                    min
    in
    List.sum <| List.map toInt list


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
