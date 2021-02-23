module Model.Worksheet.Health exposing
    ( Answers
    , Modifier
    , ModifierView
    , base
    , compute
    , modifier
    , modifiers
    , viewModifier
    )

import Model.Worksheet.Shade as Shade exposing (Shade)
import Model.Worksheet.ShadedStats as ShadedStats exposing (ShadedStats)


type alias Answers =
    { squalor : Bool
    , frail : Bool
    , wounded : Bool
    , enslaved : Bool
    , immortal : Bool
    , athletic : Bool
    , soundOfMusic : Bool
    }


compute : ShadedStats -> Answers -> ( Shade, Int )
compute stats answers =
    Tuple.mapSecond
        (\val -> val + modifier answers)
        (base stats)


base : ShadedStats -> ( Shade, Int )
base stats =
    let
        val : Int
        val =
            floor <| toFloat (stats.will.value + stats.forte.value) / 2
    in
    case ( stats.will.shade, stats.forte.shade ) of
        ( Shade.Gray, Shade.Gray ) ->
            ( Shade.Gray, val )

        ( Shade.Black, Shade.Black ) ->
            ( Shade.Black, val )

        _ ->
            ( Shade.Black, val + 2 )


type alias Modifier =
    { value : Answers -> Maybe Int
    , question : String
    , update : Maybe (Answers -> Bool -> Answers)
    }


type alias ModifierView =
    { value : Maybe Int
    , question : String
    , update : Maybe (Bool -> Answers)
    }


viewModifier : Answers -> Modifier -> ModifierView
viewModifier answers mod =
    { value = mod.value answers
    , question = mod.question
    , update = Maybe.map (\f -> f answers) mod.update
    }


modifiers : List Modifier
modifiers =
    [ { question = "Does the character live in squalor or filth? Subtract\u{00A0}1."
      , value =
            \answers ->
                if answers.squalor then
                    Just -1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | squalor = checked }
      }
    , { question = "Is the character frail or sickly? Subtract\u{00A0}1."
      , value =
            \answers ->
                if answers.frail then
                    Just -1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | frail = checked }
      }
    , { question = "Was the character severely wounded in the past? Subtract\u{00A0}1."
      , value =
            \answers ->
                if answers.wounded then
                    Just -1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | wounded = checked }
      }
    , { question = "Has the character been tortured and enslaved? Subtract\u{00A0}1."
      , value =
            \answers ->
                if answers.enslaved then
                    Just -1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | enslaved = checked }
      }
    , { question = "Is the character a Dwarf, Elf, or Orc? Add\u{00A0}1."
      , value =
            \answers ->
                if answers.immortal then
                    Just 1

                else
                    Nothing
      , update = Nothing
      }
    , { question = "Is the character athletic and active? Add\u{00A0}1."
      , value =
            \answers ->
                if answers.athletic then
                    Just 1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | athletic = checked }
      }
    , { question = "Does the character live in a really clean and happy place, like the hills in The Sound of Music? Add\u{00A0}1."
      , value =
            \answers ->
                if answers.soundOfMusic then
                    Just 1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | soundOfMusic = checked }
      }
    ]


modifier : Answers -> Int
modifier answers =
    modifiers
        |> List.filterMap (\m -> m.value answers)
        |> List.sum
