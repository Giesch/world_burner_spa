module Model.Worksheet.Shade exposing (..)


type Shade
    = Black
    | Gray


toggle : Shade -> Shade
toggle shade =
    case shade of
        Black ->
            Gray

        Gray ->
            Black


toString : Shade -> String
toString shade =
    case shade of
        Black ->
            "B"

        Gray ->
            "G"
