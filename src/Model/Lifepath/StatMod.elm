module Model.Lifepath.StatMod exposing (StatMod(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type StatMod
    = Physical Int
    | Mental Int
    | Either Int
    | Both Int
    | NoMod


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
            Decode.succeed NoMod

        k ->
            Decode.fail <| "Invalid stat mod kind: " ++ k


modVariantDecoder : (Int -> StatMod) -> Decoder StatMod
modVariantDecoder variant =
    Decode.map variant (Decode.field "value" Decode.int)
