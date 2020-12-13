module Model.Lifepath.Resources exposing
    ( Resources
    , decoder
    , points
    , toString
    )

import Json.Decode as Decode exposing (Decoder)


type Resources
    = Points Int
    | Calc ResCalc


points : Int -> Resources
points pts =
    Points pts


type ResCalc
    = HalfPrevious
    | TenPerYear


toString : Resources -> String
toString res =
    case res of
        Points p ->
            String.fromInt p ++ " res"

        Calc HalfPrevious ->
            "half prev. path res"

        Calc TenPerYear ->
            "10 res/year"


decoder : Decoder Resources
decoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen resKindsDecoder


resKindsDecoder : String -> Decoder Resources
resKindsDecoder kind =
    case kind of
        "points" ->
            Decode.map Points (Decode.field "value" Decode.int)

        "calc" ->
            Decode.map Calc calcDecoder

        k ->
            Decode.fail <| "Invalid res kind: " ++ k


calcDecoder : Decoder ResCalc
calcDecoder =
    Decode.field "value" Decode.string
        |> Decode.andThen calcFromString


calcFromString : String -> Decoder ResCalc
calcFromString val =
    case val of
        "halfPrevious" ->
            Decode.succeed HalfPrevious

        "tenPerYear" ->
            Decode.succeed TenPerYear

        k ->
            Decode.fail <| "Invalid res calc: " ++ k
