module Model.Worksheet.Stat exposing
    ( Shade(..)
    , Stat(..)
    , toString
    , toggleShade
    )


type Stat
    = Will
    | Perception
    | Power
    | Forte
    | Agility
    | Speed


type Shade
    = Black
    | Gray


toggleShade : Shade -> Shade
toggleShade shade =
    case shade of
        Black ->
            Gray

        Gray ->
            Black


toString : Stat -> String
toString stat =
    case stat of
        Will ->
            "Will"

        Perception ->
            "Perception"

        Power ->
            "Power"

        Forte ->
            "Forte"

        Agility ->
            "Agility"

        Speed ->
            "Speed"
