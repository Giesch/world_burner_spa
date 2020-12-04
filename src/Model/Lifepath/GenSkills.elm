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
            genSkillPointsDecoder

        "calc" ->
            genSkillCalcDecoder

        k ->
            Decode.fail <| "Invalid gen skill kind: " ++ k


genSkillPointsDecoder : Decoder GenSkills
genSkillPointsDecoder =
    Decode.map Points (Decode.field "value" Decode.int)


genSkillCalcDecoder : Decoder GenSkills
genSkillCalcDecoder =
    -- TODO actually match on the string
    Decode.map Calc <| Decode.succeed OnePerYear
