module Model.Lifepath.GenSkills exposing
    ( GenSkills
    , decoder
    , points
    , toString
    )

import Json.Decode as Decode exposing (Decoder)


type GenSkills
    = Points Int
    | Calc GenSkillCalc


type GenSkillCalc
    = OnePerYear


points : Int -> GenSkills
points pts =
    Points pts


toString : GenSkills -> Maybe String
toString skills =
    case skills of
        Points pts ->
            if pts > 0 then
                Just <| String.fromInt pts ++ " pts: General"

            else
                Nothing

        Calc calc ->
            case calc of
                OnePerYear ->
                    Just <| "one per year"


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
