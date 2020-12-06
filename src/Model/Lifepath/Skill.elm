module Model.Lifepath.Skill exposing
    ( Skill
    , decode
    , toString
    )

import Element exposing (Element)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import String.Extra exposing (toTitleCase)


type alias Skill =
    { skillId : Int
    , displayName : String
    , magical : Bool
    , training : Bool
    }


toString : Skill -> String
toString skill =
    skill
        |> .displayName
        |> nonBreakingHyphens
        |> toTitleCase


nonBreakingHyphens : String -> String
nonBreakingHyphens =
    String.map <|
        \c ->
            if c == '-' then
                Char.fromCode 8209

            else
                c


decode : Decoder Skill
decode =
    Decode.succeed Skill
        |> required "skillId" Decode.int
        |> required "displayName" Decode.string
        |> required "magical" Decode.bool
        |> required "training" Decode.bool
