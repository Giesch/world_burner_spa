module Model.Worksheet exposing
    ( Stats
    , Worksheet
    , age
    , changeStat
    , distributeStats
    , lifepaths
    , new
    , replaceLifepaths
    , toggleShade
    , updateLifepaths
    , view
    )

{-| A module for the data that the user can edit after choosing lifepaths.
-}

import Colors
import Common exposing (corners, edges)
import Components
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Lifepath.StatMod as StatMod exposing (StatMod)
import Model.Lifepath.Validation as Validation exposing (PathWithWarnings, ValidatedLifepaths)
import Model.Lifepath.Years as Years
import Model.Worksheet.Constants as Constants
import Model.Worksheet.Shade as Shade exposing (Shade)
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


type alias StatWithShade =
    { value : Int
    , shade : Shade
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
    { will = { value = 0, shade = Shade.Black }
    , perception = { value = 0, shade = Shade.Black }
    , power = { value = 0, shade = Shade.Black }
    , forte = { value = 0, shade = Shade.Black }
    , agility = { value = 0, shade = Shade.Black }
    , speed = { value = 0, shade = Shade.Black }
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


updateShade : StatWithShade -> Shade -> StatWithShade
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
                    { currentStats | will = { value = clampedVal, shade = Shade.Black } }

                Stat.Perception ->
                    { currentStats | perception = { value = clampedVal, shade = Shade.Black } }

                Stat.Power ->
                    { currentStats | power = { value = clampedVal, shade = Shade.Black } }

                Stat.Forte ->
                    { currentStats | forte = { value = clampedVal, shade = Shade.Black } }

                Stat.Agility ->
                    { currentStats | agility = { value = clampedVal, shade = Shade.Black } }

                Stat.Speed ->
                    { currentStats | speed = { value = clampedVal, shade = Shade.Black } }

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
    { will = { value = 1, shade = Shade.Black }
    , perception = { value = 1, shade = Shade.Black }
    , power = { value = 1, shade = Shade.Black }
    , forte = { value = 1, shade = Shade.Black }
    , agility = { value = 1, shade = Shade.Black }
    , speed = { value = 1, shade = Shade.Black }
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
        inRange : Constants.AgeTableRow -> Bool
        inRange { minAge, maxAge } =
            minAge <= a && maxAge >= a

        ageRowToBonus : Constants.AgeTableRow -> StatMod.Bonus
        ageRowToBonus { physical, mental } =
            { physical = physical, mental = mental, either = 0 }
    in
    Constants.dwarfAgeTable
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
        currentShade : Shade
        currentShade =
            sheet.stats
                |> statWithShade stat
                |> .shade

        modification : Int
        modification =
            case currentShade of
                Shade.Black ->
                    -5

                Shade.Gray ->
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

        newShade : Shade
        newShade =
            Shade.toggle currentShade

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


mortalWound : WorksheetData -> ( Shade, Int )
mortalWound sheet =
    let
        avg : Float
        avg =
            (toFloat <| sheet.stats.forte.value + sheet.stats.power.value) / 2

        base : Int
        base =
            -- NOTE ceiling or floor should depend on traits
            6 + ceiling avg
    in
    case ( sheet.stats.power.shade, sheet.stats.forte.shade ) of
        ( Shade.Black, Shade.Black ) ->
            ( Shade.Black, base )

        ( Shade.Gray, Shade.Gray ) ->
            ( Shade.Gray, base )

        _ ->
            ( Shade.Black, base + 2 )


reflexes : WorksheetData -> ( Shade, Int )
reflexes sheet =
    let
        base : Int
        base =
            (sheet.stats.perception.value
                + sheet.stats.agility.value
                + sheet.stats.speed.value
            )
                // 3

        countGray : Shade -> Int
        countGray shade =
            if shade == Shade.Gray then
                1

            else
                0

        grays : Int
        grays =
            List.sum <|
                List.map countGray
                    [ sheet.stats.perception.shade
                    , sheet.stats.agility.shade
                    , sheet.stats.speed.shade
                    ]
    in
    if grays == 3 then
        ( Shade.Gray, base )

    else
        ( Shade.Black, base + (grays * 2) )


type alias Options msg =
    { worksheet : Worksheet
    , distributeStats : msg
    , toggleShade : Stat -> msg
    , changeStat : Stat -> Int -> msg
    }


view : Options msg -> List (Element msg)
view opts =
    let
        (Worksheet sheet) =
            opts.worksheet

        viewRemaining : (StatMod.Bonus -> Int) -> String
        viewRemaining prop =
            sheet.statsRemaining
                |> Tuple.mapBoth prop prop
                |> Tuple.mapBoth String.fromInt String.fromInt
                |> (\( rem, total ) -> rem ++ "/" ++ total)

        remaining : (StatMod.Bonus -> Int) -> Int
        remaining prop =
            sheet.statsRemaining
                |> Tuple.first
                |> prop

        statValue : (Stats -> StatWithShade) -> Int
        statValue stat =
            .value (stat sheet.stats)

        statShade : (Stats -> StatWithShade) -> Shade
        statShade stat =
            .shade (stat sheet.stats)

        statRows : List StatRow
        statRows =
            [ { stat = Stat.Will
              , value = statValue .will
              , shade = statShade .will
              }
            , { stat = Stat.Perception
              , value = statValue .perception
              , shade = statShade .perception
              }
            , { stat = Stat.Power
              , value = statValue .power
              , shade = statShade .power
              }
            , { stat = Stat.Forte
              , value = statValue .forte
              , shade = statShade .forte
              }
            , { stat = Stat.Agility
              , value = statValue .agility
              , shade = statShade .agility
              }
            , { stat = Stat.Speed
              , value = statValue .speed
              , shade = statShade .speed
              }
            ]

        statWarning : (StatMod.Bonus -> Int) -> Element msg
        statWarning prop =
            el
                [ alignTop
                , alignLeft
                , transparent (remaining prop >= 0)
                , paddingEach { edges | left = 20 }
                ]
                Components.warningIcon

        viewAttribute : String -> ( Shade, Int ) -> Element msg
        viewAttribute name ( shade, value ) =
            text <| name ++ ": " ++ Shade.toString shade ++ String.fromInt value
    in
    [ el [ padding 20 ] <|
        Components.faintButton "Distribute" (Just opts.distributeStats)
    , row []
        [ table [ Font.size 18, spacing 5, padding 20 ]
            { data = statRows
            , columns =
                [ { header = none
                  , width = shrink
                  , view = text << Stat.toString << .stat
                  }
                , { header = none
                  , width = shrink
                  , view = statShadeButton opts.toggleShade
                  }
                , { header = none
                  , width = shrink
                  , view = text << String.fromInt << .value
                  }
                , { header = none
                  , width = shrink
                  , view = changeStatButtons opts.changeStat
                  }
                ]
            }
        , column
            [ width fill
            , alignTop
            , Font.size 18
            , padding 20
            , spacing 5
            ]
            [ text "Remaining: "
            , row [ width fill ]
                [ text <| "Mental: " ++ viewRemaining .mental
                , statWarning .mental
                ]
            , row []
                [ text <| "Physical: " ++ viewRemaining .physical
                , statWarning .physical
                ]
            , row [] [ text <| "Either: " ++ viewRemaining .either ]
            ]
        , column
            [ width fill
            , alignTop
            , Font.size 18
            , padding 20
            , spacing 5
            ]
            [ text "Attributes:"
            , viewAttribute "Mortal Wound" <| mortalWound sheet
            , viewAttribute "Reflexes" <| reflexes sheet
            ]
        ]
    ]


type alias StatRow =
    { stat : Stat
    , value : Int
    , shade : Shade
    }


statShadeButton : (Stat -> msg) -> StatRow -> Element msg
statShadeButton toggle { stat, shade } =
    Input.button
        [ Border.color Colors.shadow
        , Border.rounded 3
        , Border.width 1
        , Font.size 14
        , Font.family [ Font.monospace ]
        , padding 3
        ]
        { onPress = Just <| toggle stat
        , label = text <| Shade.toString shade
        }


changeStatButtons : (Stat -> Int -> msg) -> StatRow -> Element msg
changeStatButtons changeMsg statRow =
    let
        buttonStyles : List (Attribute msg)
        buttonStyles =
            [ Border.color Colors.shadow
            , Font.size 14
            , padding 3
            ]
    in
    row []
        [ Input.button
            ([ Border.roundEach { corners | topLeft = 3, bottomLeft = 3 }
             , Border.widthEach { left = 1, top = 1, bottom = 1, right = 0 }
             ]
                ++ buttonStyles
            )
            { onPress = Just <| changeMsg statRow.stat (statRow.value + 1)
            , label = text "+"
            }
        , Input.button
            ([ Border.roundEach { corners | topRight = 3, bottomRight = 3 }
             , Border.widthEach { left = 1, top = 1, bottom = 1, right = 1 }
             ]
                ++ buttonStyles
            )
            { onPress = Just <| changeMsg statRow.stat (statRow.value - 1)
            , label = text "-"
            }
        ]
