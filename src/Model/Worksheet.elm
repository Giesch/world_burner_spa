module Model.Worksheet exposing
    ( Stats
    , Worksheet
    , age
    , lifepaths
    , new
    , replaceLifepaths
    , statsRemaining
    , updateLifepaths
    )

{-| A module for the data that the user can edit after choosing lifepaths.
-}

import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Lifepath.StatMod as StatMod exposing (StatMod)
import Model.Lifepath.Validation as Validation exposing (PathWithWarnings, ValidatedLifepaths)
import Model.Lifepath.Years as Years


type Worksheet
    = Worksheet WorksheetData


type alias WorksheetData =
    { lifepaths : ValidatedLifepaths
    , age : Int
    , statsRemaining : StatMod.Bonus
    , stats : Stats
    }


age : Worksheet -> Int
age (Worksheet sheet) =
    sheet.age


statsRemaining : Worksheet -> StatMod.Bonus
statsRemaining (Worksheet sheet) =
    sheet.statsRemaining


type alias Stats =
    { will : Int
    , perception : Int
    , power : Int
    , forte : Int
    , agility : Int
    , speed : Int
    }


zeroStats : Stats
zeroStats =
    { will = 0
    , perception = 0
    , power = 0
    , forte = 0
    , agility = 0
    , speed = 0
    }


lifepaths : Worksheet -> List PathWithWarnings
lifepaths (Worksheet sheet) =
    sheet.lifepaths
        |> Validation.unpack
        |> NonEmpty.toList


updateLifepaths :
    (List PathWithWarnings -> List PathWithWarnings)
    -> Worksheet
    -> Maybe Worksheet
updateLifepaths update workSheet =
    workSheet
        |> lifepaths
        |> update
        |> (\paths -> replaceLifepaths paths workSheet)


replaceLifepaths : List PathWithWarnings -> Worksheet -> Maybe Worksheet
replaceLifepaths paths (Worksheet sheet) =
    paths
        |> List.map .lifepath
        |> NonEmpty.fromList
        |> Maybe.map new


new : NonEmpty Lifepath -> Worksheet
new paths =
    Worksheet <| newData paths


newData : NonEmpty Lifepath -> WorksheetData
newData paths =
    let
        newAge : Int
        newAge =
            sumAge paths

        remaining : StatMod.Bonus
        remaining =
            StatMod.addBonus (ageStats newAge) (lifepathBonuses paths)
    in
    { lifepaths = Validation.addWarnings paths
    , age = newAge
    , statsRemaining = remaining
    , stats = zeroStats
    }


sumAge : NonEmpty Lifepath -> Int
sumAge paths =
    paths
        |> NonEmpty.toList
        |> List.map .years
        |> Years.age


ageStats : Int -> StatMod.Bonus
ageStats a =
    let
        inRange : AgeTableRow -> Bool
        inRange { minAge, maxAge } =
            minAge <= a && maxAge >= a

        ageRowToBonus : AgeTableRow -> StatMod.Bonus
        ageRowToBonus { physical, mental } =
            { physical = physical, mental = mental, either = 0 }
    in
    dwarfAgeTable
        |> List.filter inRange
        |> List.head
        |> Maybe.map ageRowToBonus
        |> Maybe.withDefault StatMod.noBonus


lifepathBonuses : NonEmpty Lifepath -> StatMod.Bonus
lifepathBonuses =
    let
        sumBonus path bonus =
            StatMod.addBonus bonus <| StatMod.bonus <| path.statMod
    in
    NonEmpty.foldl sumBonus StatMod.noBonus


type alias AgeTableRow =
    { minAge : Int
    , maxAge : Int
    , physical : Int
    , mental : Int
    }


dwarfAgeTable : List AgeTableRow
dwarfAgeTable =
    [ { minAge = 1
      , maxAge = 20
      , physical = 6
      , mental = 13
      }
    , { minAge = 21
      , maxAge = 30
      , physical = 7
      , mental = 13
      }
    , { minAge = 31
      , maxAge = 50
      , physical = 7
      , mental = 14
      }
    , { minAge = 51
      , maxAge = 76
      , physical = 8
      , mental = 15
      }
    , { minAge = 77
      , maxAge = 111
      , physical = 8
      , mental = 16
      }
    , { minAge = 112
      , maxAge = 151
      , physical = 9
      , mental = 16
      }
    , { minAge = 152
      , maxAge = 199
      , physical = 9
      , mental = 17
      }
    , { minAge = 200
      , maxAge = 245
      , physical = 10
      , mental = 18
      }
    , { minAge = 246
      , maxAge = 300
      , physical = 11
      , mental = 17
      }
    , { minAge = 301
      , maxAge = 345
      , physical = 11
      , mental = 16
      }
    , { minAge = 346
      , maxAge = 396
      , physical = 12
      , mental = 15
      }
    , { minAge = 397
      , maxAge = 445
      , physical = 11
      , mental = 14
      }
    , { minAge = 446
      , maxAge = 525
      , physical = 11
      , mental = 13
      }
    , { minAge = 526
      , maxAge = 600
      , physical = 10
      , mental = 12
      }
    ]
