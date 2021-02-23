module Model.Worksheet.Steel exposing (..)

import Model.Worksheet.Shade as Shade exposing (Shade)
import Model.Worksheet.ShadedStats as ShadedStats exposing (ShadedStats)


type alias Answers =
    { soldier : Bool
    , wounded : Bool
    , killer : Bool
    , enslaved : Bool
    , sheltered : Bool
    , competitiveCulture : Bool
    , givenBirth : Bool
    , gifted : Bool
    }


type alias Modifier =
    { value : ( ShadedStats, Answers ) -> Maybe Int
    , question : String
    , update : Maybe (Answers -> Bool -> Answers)
    }


type alias ModifierView =
    { value : Maybe Int
    , question : String
    , update : Maybe (Bool -> Answers)
    }


viewModifier : ( ShadedStats, Answers ) -> Modifier -> ModifierView
viewModifier ( stats, answers ) mod =
    { value = mod.value ( stats, answers )
    , question = mod.question
    , update = Maybe.map (\f -> f answers) mod.update
    }


modifiers : List Modifier
modifiers =
    [ { question = "Has the character taken a conscript, soldier, bandit, squire, or knight type lifepath? Add 1."
      , value =
            \( _, answers ) ->
                if answers.soldier then
                    Just 1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | soldier = checked }
      }
    , { question = "Has the character ever been severly wounded? If they were, and were a soldier, knight, bandit, etc., add one. If they were wounded, but not a soldier, subtract 1."
      , value =
            \( _, answers ) ->
                if answers.wounded && answers.soldier then
                    Just 1

                else if answers.wounded && not answers.soldier then
                    Just -1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | wounded = checked }
      }
    , { question = "Has the character ever murdered or killed with their own hand? If they have done so more than once, add 1."
      , value =
            \( _, answers ) ->
                if answers.killer then
                    Just 1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | killer = checked }
      }
    , { question = "Has the character been tortured, enslaved, or beaten terribly over time? If yes, and their Will is 5 or higher, add 1. If yes, and their Will is 3 or lower, subtract 1."
      , value =
            \( stats, answers ) ->
                if answers.enslaved && stats.will.value >= 5 then
                    Just 1

                else if answers.enslaved && stats.will.value <= 3 then
                    Just -1

                else if answers.enslaved then
                    Just 0

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | enslaved = checked }
      }
    , { question = "Has the character led a sheltered life, free from violence and pain? Subtract 1."
      , value =
            \( _, answers ) ->
                if answers.sheltered then
                    Just -1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | sheltered = checked }
      }
    , { question = "Has the character been raised in a competitive (but non-violent) culture? Add 1."
      , value =
            \( _, answers ) ->
                if answers.competitiveCulture then
                    Just 1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | competitiveCulture = checked }
      }
    , { question = "Has the character given birth to a child? Add 1."
      , value =
            \( _, answers ) ->
                if answers.givenBirth then
                    Just 1

                else
                    Nothing
      , update = Just <| \answers checked -> { answers | givenBirth = checked }
      }
    , { question = "Is the character Gifted or Faithful?"
      , value =
            -- NOTE: this should reference the trait list, once that's available
            \( _, answers ) ->
                if answers.gifted then
                    Just 1

                else
                    Nothing
      , update = Nothing
      }
    , { question = "Is the character's Perception 6 or higher? Add 1."
      , value =
            \( stats, _ ) ->
                if stats.perception.value >= 6 then
                    Just 1

                else
                    Nothing
      , update = Nothing
      }
    , { question = "If the character's Will is 5 or higher, add 1. If the character's Will is 7 or higher, add 2 instead."
      , value =
            \( { will }, _ ) ->
                if will.value >= 5 && will.value < 7 then
                    Just 1

                else if will.value >= 7 then
                    Just 2

                else
                    Nothing
      , update = Nothing
      }
    , { question = "If the character's Forte is 6 or higher, add 1."
      , value =
            \( { forte }, _ ) ->
                if forte.value >= 6 then
                    Just 1

                else
                    Nothing
      , update = Nothing
      }
    ]


value : ShadedStats -> Answers -> Int
value stats answers =
    base + modifier stats answers


modifier : ShadedStats -> Answers -> Int
modifier stats answers =
    modifiers
        |> List.filterMap (\m -> m.value ( stats, answers ))
        |> List.sum


base : Int
base =
    3
