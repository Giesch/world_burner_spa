module Model.Lifepath.GenSkills exposing (GenSkills(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type GenSkills
    = Points Int
    | Calc GenSkillCalc


type GenSkillCalc
    = OnePerYear


decoder : Decoder GenSkills
decoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen kindsDecoder


kindsDecoder : String -> Decoder GenSkills
kindsDecoder kind =
    case kind of
        "points" ->
            Decode.map Points (Decode.field "value" Decode.int)

        "calc" ->
            Decode.map Calc calcDecoder

        k ->
            Decode.fail <| "Invalid gen skill kind: " ++ k


calcDecoder : Decoder GenSkillCalc
calcDecoder =
    Decode.field "value" Decode.string
        |> Decode.andThen calcFromString


calcFromString : String -> Decoder GenSkillCalc
calcFromString val =
    case val of
        "onePerYear" ->
            Decode.succeed OnePerYear

        _ ->
            Decode.fail <| "Invalid res calc: " ++ val
