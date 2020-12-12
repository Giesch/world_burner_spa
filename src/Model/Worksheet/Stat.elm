module Model.Worksheet.Stat exposing (Stat(..), toString)


type Stat
    = Will
    | Perception
    | Power
    | Forte
    | Agility
    | Speed


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
