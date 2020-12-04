module Model.Skill exposing (Skill, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Skill =
    { id : Int
    , name : String
    , magical : Bool
    , training : Bool
    }


decoder : Decoder Skill
decoder =
    Decode.succeed Skill
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "magical" Decode.bool
        |> required "training" Decode.bool
