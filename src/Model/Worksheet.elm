module Model.Worksheet exposing
    ( Stats
    , Worksheet
    , age
    , changeStat
    , distributeStats
    , lifepaths
    , new
    , replaceLifepaths
    , stats
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
import Model.Worksheet.Stat as Stat exposing (Stat)


type Worksheet
    = Worksheet WorksheetData


type alias WorksheetData =
    { lifepaths : ValidatedLifepaths
    , age : Int
    , statsRemaining : ( StatMod.Bonus, StatMod.Bonus )
    , stats : Stats
    }


age : Worksheet -> Int
age (Worksheet sheet) =
    sheet.age


statsRemaining : Worksheet -> ( StatMod.Bonus, StatMod.Bonus )
statsRemaining (Worksheet sheet) =
    sheet.statsRemaining


stats : Worksheet -> Stats
stats (Worksheet sheet) =
    sheet.stats


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


updateLifepaths : (List Lifepath -> List Lifepath) -> Worksheet -> Maybe Worksheet
updateLifepaths updateFn workSheet =
    workSheet
        |> lifepaths
        |> List.map .lifepath
        |> updateFn
        |> NonEmpty.fromList
        |> Maybe.map (\paths -> replaceLifepaths paths workSheet)


replaceLifepaths : NonEmpty Lifepath -> Worksheet -> Worksheet
replaceLifepaths paths (Worksheet sheet) =
    let
        lifepathData : LifepathData
        lifepathData =
            recalculateLifepathData sheet.stats paths
    in
    Worksheet
        { sheet
            | lifepaths = lifepathData.lifepaths
            , age = lifepathData.age
            , statsRemaining = lifepathData.statsRemaining
        }


distributeStats : Worksheet -> Worksheet
distributeStats (Worksheet sheet) =
    let
        totals : StatMod.Bonus
        totals =
            Tuple.second sheet.statsRemaining

        updatedStats =
            zeroStats
                |> distributeMental totals.mental
                |> distributePhysical totals.physical

        updatedRemaining =
            ( StatMod.noBonus, Tuple.second sheet.statsRemaining )
    in
    Worksheet { sheet | stats = updatedStats, statsRemaining = updatedRemaining }


distributeMental : Int -> Stats -> Stats
distributeMental points current =
    if points > 0 then
        if current.will <= current.perception then
            distributeMental (points - 1)
                { current | will = current.will + 1 }

        else
            distributeMental (points - 1)
                { current | perception = current.perception + 1 }

    else
        current


distributePhysical : Int -> Stats -> Stats
distributePhysical points current =
    let
        lowest : Int
        lowest =
            List.minimum [ current.power, current.forte, current.agility, current.speed ]
                |> Maybe.withDefault 0
    in
    if points > 0 then
        if current.power == lowest then
            distributePhysical (points - 1) { current | power = current.power + 1 }

        else if current.forte == lowest then
            distributePhysical (points - 1) { current | forte = current.forte + 1 }

        else if current.agility == lowest then
            distributePhysical (points - 1) { current | agility = current.agility + 1 }

        else if current.speed == lowest then
            distributePhysical (points - 1) { current | speed = current.speed + 1 }

        else
            current

    else
        current


changeStat : Stat -> Int -> Worksheet -> Worksheet
changeStat stat val (Worksheet sheet) =
    let
        clampedVal =
            if val < 1 then
                1

            else
                val

        currentStats =
            sheet.stats

        updatedStats =
            case stat of
                Stat.Will ->
                    { currentStats | will = clampedVal }

                Stat.Perception ->
                    { currentStats | perception = clampedVal }

                Stat.Power ->
                    { currentStats | power = clampedVal }

                Stat.Forte ->
                    { currentStats | forte = clampedVal }

                Stat.Agility ->
                    { currentStats | agility = clampedVal }

                Stat.Speed ->
                    { currentStats | speed = clampedVal }

        updatedRemaining =
            recalculateSpentStats updatedStats <| Tuple.second sheet.statsRemaining
    in
    Worksheet
        { sheet
            | stats = updatedStats
            , statsRemaining = updatedRemaining
        }


recalculateSpentStats : Stats -> StatMod.Bonus -> ( StatMod.Bonus, StatMod.Bonus )
recalculateSpentStats currentStats total =
    let
        subtractStats : StatMod.Bonus -> StatMod.Bonus
        subtractStats current =
            { mental = current.mental - (currentStats.will + currentStats.perception)
            , physical =
                current.physical
                    - List.sum
                        [ currentStats.power
                        , currentStats.forte
                        , currentStats.agility
                        , currentStats.speed
                        ]
            , either = current.either
            }

        spendEither : StatMod.Bonus -> StatMod.Bonus
        spendEither current =
            if current.either <= 0 then
                current

            else if current.mental < 0 then
                spendEither
                    { mental = current.mental + 1
                    , physical = current.physical
                    , either = current.either - 1
                    }

            else if current.physical < 0 then
                spendEither
                    { mental = current.mental
                    , physical = current.physical + 1
                    , either = current.either - 1
                    }

            else
                current
    in
    ( total
        |> subtractStats
        |> spendEither
    , total
    )


new : NonEmpty Lifepath -> Worksheet
new paths =
    Worksheet <| newData paths


newData : NonEmpty Lifepath -> WorksheetData
newData paths =
    let
        lifepathData =
            recalculateLifepathData allOnes paths
    in
    { lifepaths = lifepathData.lifepaths
    , age = lifepathData.age
    , statsRemaining = lifepathData.statsRemaining
    , stats = allOnes
    }


{-| Lifepaths and data calculated directly from lifepaths.
-}
type alias LifepathData =
    { lifepaths : ValidatedLifepaths
    , age : Int
    , statsRemaining : ( StatMod.Bonus, StatMod.Bonus )
    }


recalculateLifepathData : Stats -> NonEmpty Lifepath -> LifepathData
recalculateLifepathData currentStats paths =
    let
        newAge : Int
        newAge =
            sumAge paths

        totalStats : StatMod.Bonus
        totalStats =
            StatMod.addBonus (ageStats newAge) (lifepathBonuses paths)

        recalculatedStatsRemaining : ( StatMod.Bonus, StatMod.Bonus )
        recalculatedStatsRemaining =
            recalculateSpentStats currentStats totalStats
    in
    { lifepaths = Validation.addWarnings paths
    , age = newAge
    , statsRemaining = recalculatedStatsRemaining
    }


allOnes : Stats
allOnes =
    { will = 1
    , perception = 1
    , power = 1
    , forte = 1
    , agility = 1
    , speed = 1
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
            StatMod.addBonus bonus <| StatMod.bonus path.statMod
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
      , mental = 6
      , physical = 13
      }
    , { minAge = 21
      , maxAge = 30
      , mental = 7
      , physical = 13
      }
    , { minAge = 31
      , maxAge = 50
      , mental = 7
      , physical = 14
      }
    , { minAge = 51
      , maxAge = 76
      , mental = 8
      , physical = 15
      }
    , { minAge = 77
      , maxAge = 111
      , mental = 8
      , physical = 16
      }
    , { minAge = 112
      , maxAge = 151
      , mental = 9
      , physical = 16
      }
    , { minAge = 152
      , maxAge = 199
      , mental = 9
      , physical = 17
      }
    , { minAge = 200
      , maxAge = 245
      , mental = 10
      , physical = 18
      }
    , { minAge = 246
      , maxAge = 300
      , mental = 11
      , physical = 17
      }
    , { minAge = 301
      , maxAge = 345
      , mental = 11
      , physical = 16
      }
    , { minAge = 346
      , maxAge = 396
      , mental = 12
      , physical = 15
      }
    , { minAge = 397
      , maxAge = 445
      , mental = 11
      , physical = 14
      }
    , { minAge = 446
      , maxAge = 525
      , mental = 11
      , physical = 13
      }
    , { minAge = 526
      , maxAge = 600
      , mental = 10
      , physical = 12
      }
    ]
