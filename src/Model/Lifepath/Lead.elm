module Model.Lifepath.Lead exposing (Lead, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Lead =
    { settingName : String
    , settingId : Int
    }


decoder : Decoder Lead
decoder =
    Decode.succeed Lead
        |> required "settingName" Decode.string
        |> required "settingId" Decode.int
