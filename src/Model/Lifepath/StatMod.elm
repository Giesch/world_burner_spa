module Model.Lifepath.StatMod exposing (StatMod, decoder, toString)

import Common.NonZero as NonZero exposing (NonZero)
import Json.Decode as Decode exposing (Decoder)


type StatMod
    = Physical NonZero
    | Mental NonZero
    | Either NonZero
    | Both NonZero
    | None


toString : StatMod -> String
toString statMod =
    case statMod of
        None ->
            "stat: --"

        Physical val ->
            "stat: " ++ viewVal val ++ "P"

        Mental val ->
            "stat: " ++ viewVal val ++ "M"

        Either val ->
            "stat: " ++ viewVal val ++ "M/P"

        Both val ->
            "stat: " ++ viewVal val ++ "M,P"


viewVal : NonZero -> String
viewVal val =
    let
        v =
            NonZero.toInt val
    in
    if v > 0 then
        "+" ++ String.fromInt v

    else
        "-" ++ String.fromInt v


decoder : Decoder StatMod
decoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen statModKindsDecoder


statModKindsDecoder : String -> Decoder StatMod
statModKindsDecoder kind =
    case kind of
        "physical" ->
            modVariantDecoder Physical

        "mental" ->
            modVariantDecoder Mental

        "either" ->
            modVariantDecoder Either

        "both" ->
            modVariantDecoder Both

        "none" ->
            Decode.succeed None

        _ ->
            Decode.fail <| "Invalid stat mod kind: " ++ kind


modVariantDecoder : (NonZero -> StatMod) -> Decoder StatMod
modVariantDecoder variant =
    Decode.map variant (Decode.field "value" NonZero.decode)
