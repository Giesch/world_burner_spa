module Model.Lifepath.Skill exposing (Skill, decode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


type alias Skill =
    { skillId : Int
    , displayName : String
    , magical : Bool
    , training : Bool
    }


decode : Decoder Skill
decode =
    Decode.succeed Skill
        |> required "skillId" Decode.int
        |> required "displayName" Decode.string
        |> required "magical" Decode.bool
        |> required "training" Decode.bool
