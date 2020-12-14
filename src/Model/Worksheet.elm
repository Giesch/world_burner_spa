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
    , toggleShade
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


type alias StatWithShade =
    { value : Int
    , shade : Stat.Shade
    }


type alias Stats =
    { will : StatWithShade
    , perception : StatWithShade
    , power : StatWithShade
    , forte : StatWithShade
    , agility : StatWithShade
    , speed : StatWithShade
    }


statWithShade : Stat -> Stats -> StatWithShade
statWithShade stat sheetStats =
    case stat of
        Stat.Will ->
            sheetStats.will

        Stat.Perception ->
            sheetStats.perception

        Stat.Power ->
            sheetStats.power

        Stat.Forte ->
            sheetStats.forte

        Stat.Agility ->
            sheetStats.agility

        Stat.Speed ->
            sheetStats.speed


zeroStats : Stats
zeroStats =
    { will = { value = 0, shade = Stat.Black }
    , perception = { value = 0, shade = Stat.Black }
    , power = { value = 0, shade = Stat.Black }
    , forte = { value = 0, shade = Stat.Black }
    , agility = { value = 0, shade = Stat.Black }
    , speed = { value = 0, shade = Stat.Black }
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
        if current.will.value <= current.perception.value then
            distributeMental (points - 1)
                { current | will = updateStat current.will (current.will.value + 1) }

        else
            distributeMental (points - 1)
                { current
                    | perception =
                        updateStat current.perception
                            (current.perception.value + 1)
                }

    else
        current


updateStat : StatWithShade -> Int -> StatWithShade
updateStat stat value =
    { stat | value = value }


updateShade : StatWithShade -> Stat.Shade -> StatWithShade
updateShade stat shade =
    { stat | shade = shade }


distributePhysical : Int -> Stats -> Stats
distributePhysical points current =
    let
        lowest : Int
        lowest =
            [ current.power, current.forte, current.agility, current.speed ]
                |> List.map .value
                |> List.minimum
                |> Maybe.withDefault 0
    in
    if points > 0 then
        if current.power.value == lowest then
            distributePhysical (points - 1)
                { current | power = updateStat current.power (current.power.value + 1) }

        else if current.forte.value == lowest then
            distributePhysical (points - 1)
                { current | forte = updateStat current.forte (current.forte.value + 1) }

        else if current.agility.value == lowest then
            distributePhysical (points - 1)
                { current | agility = updateStat current.agility (current.agility.value + 1) }

        else if current.speed.value == lowest then
            distributePhysical (points - 1)
                { current | speed = updateStat current.speed (current.speed.value + 1) }

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
                    { currentStats | will = { value = clampedVal, shade = Stat.Black } }

                Stat.Perception ->
                    { currentStats | perception = { value = clampedVal, shade = Stat.Black } }

                Stat.Power ->
                    { currentStats | power = { value = clampedVal, shade = Stat.Black } }

                Stat.Forte ->
                    { currentStats | forte = { value = clampedVal, shade = Stat.Black } }

                Stat.Agility ->
                    { currentStats | agility = { value = clampedVal, shade = Stat.Black } }

                Stat.Speed ->
                    { currentStats | speed = { value = clampedVal, shade = Stat.Black } }

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
            { mental =
                current.mental
                    - (currentStats.will.value + currentStats.perception.value)
            , physical =
                current.physical
                    - List.sum
                        [ currentStats.power.value
                        , currentStats.forte.value
                        , currentStats.agility.value
                        , currentStats.speed.value
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
    { will = { value = 1, shade = Stat.Black }
    , perception = { value = 1, shade = Stat.Black }
    , power = { value = 1, shade = Stat.Black }
    , forte = { value = 1, shade = Stat.Black }
    , agility = { value = 1, shade = Stat.Black }
    , speed = { value = 1, shade = Stat.Black }
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


toggleShade : Stat -> Worksheet -> Worksheet
toggleShade stat (Worksheet sheet) =
    let
        currentShade : Stat.Shade
        currentShade =
            sheet.stats
                |> statWithShade stat
                |> .shade

        modification : Int
        modification =
            case currentShade of
                Stat.Black ->
                    -5

                Stat.Gray ->
                    5

        updatedRemaining : ( StatMod.Bonus, StatMod.Bonus )
        updatedRemaining =
            if stat == Stat.Will || stat == Stat.Perception then
                Tuple.mapFirst
                    (\bonus -> { bonus | mental = bonus.mental + modification })
                    sheet.statsRemaining

            else
                Tuple.mapFirst
                    (\bonus -> { bonus | physical = bonus.physical + modification })
                    sheet.statsRemaining

        newShade : Stat.Shade
        newShade =
            Stat.toggleShade currentShade

        currentStats : Stats
        currentStats =
            sheet.stats

        updatedStats : Stats
        updatedStats =
            case stat of
                Stat.Will ->
                    { currentStats | will = updateShade currentStats.will newShade }

                Stat.Perception ->
                    { currentStats | perception = updateShade currentStats.perception newShade }

                Stat.Power ->
                    { currentStats | power = updateShade currentStats.power newShade }

                Stat.Forte ->
                    { currentStats | forte = updateShade currentStats.forte newShade }

                Stat.Agility ->
                    { currentStats | agility = updateShade currentStats.agility newShade }

                Stat.Speed ->
                    { currentStats | speed = updateShade currentStats.speed newShade }
    in
    Worksheet { sheet | stats = updatedStats, statsRemaining = updatedRemaining }


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
