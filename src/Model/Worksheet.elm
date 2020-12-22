module Model.Worksheet exposing
    ( HealthAnswers
    , Stats
    , SteelAnswers
    , Worksheet
    , age
    , changeStat
    , distributeStats
    , healthAnswers
    , lifepaths
    , new
    , replaceLifepaths
    , steelAnswers
    , toggleShade
    , toggleSteelShade
    , updateHealthAnswers
    , updateLifepaths
    , updateSteelAnswers
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
    , healthAndSteelAnswers : HealthAndSteelAnswers
    , graySteel : Bool
    }


age : Worksheet -> Int
age (Worksheet sheet) =
    sheet.age


type alias ShadedStat =
    { value : Int
    , shade : Shade
    }


type alias Stats =
    { will : ShadedStat
    , perception : ShadedStat
    , power : ShadedStat
    , forte : ShadedStat
    , agility : ShadedStat
    , speed : ShadedStat
    }


type alias HealthAndSteelAnswers =
    { -- health
      squalor : Bool
    , frail : Bool
    , wounded : Bool -- also used by steel
    , enslaved : Bool -- also used by steel
    , immortal : Bool
    , athletic : Bool
    , soundOfMusic : Bool

    -- steel only
    , soldier : Bool
    , killer : Bool
    , sheltered : Bool
    , competitiveCulture : Bool
    , givenBirth : Bool
    , gifted : Bool
    }


type alias HealthAnswers =
    { squalor : Bool
    , frail : Bool
    , wounded : Bool
    , enslaved : Bool
    , immortal : Bool
    , athletic : Bool
    , soundOfMusic : Bool
    }


healthAnswers : Worksheet -> HealthAnswers
healthAnswers (Worksheet { healthAndSteelAnswers }) =
    { squalor = healthAndSteelAnswers.squalor
    , frail = healthAndSteelAnswers.frail
    , wounded = healthAndSteelAnswers.wounded
    , enslaved = healthAndSteelAnswers.enslaved
    , immortal = healthAndSteelAnswers.immortal
    , athletic = healthAndSteelAnswers.athletic
    , soundOfMusic = healthAndSteelAnswers.soundOfMusic
    }


type alias SteelAnswers =
    { soldier : Bool
    , wounded : Bool
    , killer : Bool
    , enslaved : Bool
    , sheltered : Bool
    , competitiveCulture : Bool
    , givenBirth : Bool
    , gifted : Bool
    }


steelAnswers : Worksheet -> SteelAnswers
steelAnswers (Worksheet { healthAndSteelAnswers }) =
    { soldier = healthAndSteelAnswers.soldier
    , wounded = healthAndSteelAnswers.wounded
    , killer = healthAndSteelAnswers.killer
    , enslaved = healthAndSteelAnswers.enslaved
    , sheltered = healthAndSteelAnswers.sheltered
    , competitiveCulture = healthAndSteelAnswers.competitiveCulture
    , givenBirth = healthAndSteelAnswers.givenBirth
    , gifted = healthAndSteelAnswers.gifted
    }


updateSteelAnswers : SteelAnswers -> Worksheet -> Worksheet
updateSteelAnswers newAnswers (Worksheet ({ healthAndSteelAnswers } as sheet)) =
    let
        fullAnswers : HealthAndSteelAnswers
        fullAnswers =
            { healthAndSteelAnswers
                | soldier = newAnswers.soldier
                , wounded = newAnswers.wounded
                , killer = newAnswers.killer
                , enslaved = newAnswers.enslaved
                , sheltered = newAnswers.sheltered
                , competitiveCulture = newAnswers.competitiveCulture
                , givenBirth = newAnswers.givenBirth
                , gifted = newAnswers.gifted
            }
    in
    Worksheet { sheet | healthAndSteelAnswers = fullAnswers }


updateHealthAnswers : HealthAnswers -> Worksheet -> Worksheet
updateHealthAnswers newAnswers (Worksheet ({ healthAndSteelAnswers } as sheet)) =
    let
        fullAnswers : HealthAndSteelAnswers
        fullAnswers =
            { healthAndSteelAnswers
                | squalor = newAnswers.squalor
                , frail = newAnswers.frail
                , wounded = newAnswers.wounded
                , enslaved = newAnswers.enslaved
                , immortal = newAnswers.immortal
                , athletic = newAnswers.athletic
                , soundOfMusic = newAnswers.soundOfMusic
            }
    in
    Worksheet { sheet | healthAndSteelAnswers = fullAnswers }


toggleSteelShade : Shade -> Worksheet -> Worksheet
toggleSteelShade shade (Worksheet sheet) =
    Worksheet { sheet | graySteel = shade == Shade.Gray }


defaultAnswers : HealthAndSteelAnswers
defaultAnswers =
    { squalor = False
    , frail = False
    , wounded = False
    , enslaved = False
    , immortal = True -- NOTE this should be based on traits
    , athletic = False
    , soundOfMusic = False
    , soldier = False
    , killer = False
    , sheltered = False
    , competitiveCulture = False
    , givenBirth = False
    , gifted = False
    }


statWithShade : Stat -> Stats -> ShadedStat
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


updateStatVal : Stat -> Int -> Stats -> Stats
updateStatVal stat value stats =
    let
        { shade } =
            statWithShade stat stats

        newStat : ShadedStat
        newStat =
            { value = value, shade = shade }
    in
    case stat of
        Stat.Will ->
            { stats | will = newStat }

        Stat.Perception ->
            { stats | perception = newStat }

        Stat.Power ->
            { stats | power = newStat }

        Stat.Forte ->
            { stats | forte = newStat }

        Stat.Agility ->
            { stats | agility = newStat }

        Stat.Speed ->
            { stats | speed = newStat }


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


updateStat : ShadedStat -> Int -> ShadedStat
updateStat stat value =
    { stat | value = value }


updateShade : ShadedStat -> Shade -> ShadedStat
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
        clampedVal : Int
        clampedVal =
            if val < 1 then
                1

            else
                val

        updatedStats : Stats
        updatedStats =
            updateStatVal stat clampedVal sheet.stats

        updatedRemaining : ( StatMod.Bonus, StatMod.Bonus )
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
        subtractShades : StatMod.Bonus -> StatMod.Bonus
        subtractShades current =
            { mental =
                [ currentStats.will.shade
                , currentStats.perception.shade
                ]
                    |> List.map Shade.countGray
                    |> List.sum
                    |> (\sum -> current.mental - (sum * 5))
            , physical =
                [ currentStats.power.shade
                , currentStats.forte.shade
                , currentStats.agility.shade
                , currentStats.speed.shade
                ]
                    |> List.map Shade.countGray
                    |> List.sum
                    |> (\sum -> current.physical - (sum * 5))
            , either = current.either
            }

        subtractStats : StatMod.Bonus -> StatMod.Bonus
        subtractStats current =
            { mental =
                current.mental
                    - (currentStats.will.value
                        + currentStats.perception.value
                      )
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
        |> subtractShades
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
    , healthAndSteelAnswers = defaultAnswers
    , graySteel = False
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
            toFloat (sheet.stats.forte.value + sheet.stats.power.value) / 2

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
        { perception, agility, speed } =
            sheet.stats

        base : Int
        base =
            (perception.value + agility.value + speed.value) // 3

        grays : Int
        grays =
            [ perception.shade, agility.shade, speed.shade ]
                |> List.map Shade.countGray
                |> List.sum
    in
    if grays == 3 then
        ( Shade.Gray, base )

    else
        ( Shade.Black, base + (grays * 2) )


health : WorksheetData -> ( Shade, Int )
health { stats, healthAndSteelAnswers } =
    let
        avg : Float
        avg =
            toFloat (stats.will.value + stats.forte.value) / 2

        base : Int
        base =
            floor avg + healthModifier healthAndSteelAnswers
    in
    case ( stats.will.shade, stats.forte.shade ) of
        ( Shade.Gray, Shade.Gray ) ->
            ( Shade.Gray, base )

        ( Shade.Black, Shade.Black ) ->
            ( Shade.Black, base )

        _ ->
            ( Shade.Black, base + 2 )


healthModifier : HealthAndSteelAnswers -> Int
healthModifier answers =
    [ ( answers.squalor, -1 )
    , ( answers.frail, -1 )
    , ( answers.wounded, -1 )
    , ( answers.enslaved, -1 )
    , ( answers.immortal, 1 )
    , ( answers.athletic, 1 )
    , ( answers.soundOfMusic, 1 )
    ]
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> List.sum


steel : WorksheetData -> ( Shade, Int )
steel sheet =
    if sheet.graySteel then
        ( Shade.Gray, steelValue sheet - 5 )

    else
        ( Shade.Black, steelValue sheet )


steelValue : WorksheetData -> Int
steelValue { healthAndSteelAnswers, stats } =
    let
        answers =
            healthAndSteelAnswers
    in
    [ ( answers.soldier, 1 )
    , ( answers.wounded && answers.soldier, 1 )
    , ( answers.wounded && not answers.soldier, -1 )
    , ( answers.killer, 1 )
    , ( answers.enslaved && stats.will.value >= 5, 1 )
    , ( answers.enslaved && stats.will.value <= 3, -1 )
    , ( answers.sheltered, -1 )
    , ( answers.competitiveCulture, 1 )
    , ( answers.givenBirth, 1 )
    , ( answers.gifted, 1 )
    , ( stats.perception.value >= 6, 1 )
    , ( stats.will.value >= 5, 1 )
    , ( stats.will.value >= 7, 1 )
    , ( stats.forte.value >= 6, 1 )
    ]
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> List.sum
        |> (+) 3


hesitation : WorksheetData -> Int
hesitation sheet =
    10 - sheet.stats.will.value


stride : WorksheetData -> Int
stride sheet =
    -- NOTE this should depend on traits/stock
    6


circles : WorksheetData -> ( Shade, Int )
circles sheet =
    -- NOTE need to factor in 50 res spent bonus
    let
        halfWill : Int
        halfWill =
            sheet.stats.will.value // 2

        value : Int
        value =
            if halfWill > 1 then
                halfWill

            else
                1
    in
    ( sheet.stats.will.shade, value )


resources : WorksheetData -> ( Shade, Int )
resources sheet =
    -- NOTE should calculate based on gear spend
    ( Shade.Black, 0 )


type alias Options msg =
    { worksheet : Worksheet
    , distributeStats : msg
    , toggleShade : Stat -> msg
    , changeStat : Stat -> Int -> msg
    , openHealthModal : msg
    , openSteelModal : msg
    , toggleSteelShade : Shade -> msg
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

        statRow : Stat -> StatRow
        statRow stat =
            let
                { value, shade } =
                    statWithShade stat sheet.stats
            in
            { stat = stat, value = value, shade = shade }

        statRows : List StatRow
        statRows =
            List.map statRow
                [ Stat.Will
                , Stat.Perception
                , Stat.Power
                , Stat.Forte
                , Stat.Agility
                , Stat.Speed
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
        Components.faintButton "Distribute" opts.distributeStats
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
            , row [ spacing 10 ]
                [ viewAttribute "Health" <| health sheet
                , questionsButton opts.openHealthModal
                ]
            , viewSteel (steel sheet) opts
            , text <| "Hesitation: " ++ String.fromInt (hesitation sheet)
            , text <| "Stride: " ++ String.fromInt (stride sheet)
            , viewAttribute "Circles" <| circles sheet
            , viewAttribute "Resources" <| resources sheet
            ]
        ]
    ]


viewSteel : ( Shade, Int ) -> Options msg -> Element msg
viewSteel ( steelShade, steelVal ) opts =
    row []
        [ text "Steel:"
        , el [ paddingEach { edges | left = 10 } ] <|
            Input.button
                [ Border.color Colors.shadow
                , Border.rounded 3
                , Border.width 1
                , Font.size 14
                , Font.family [ Font.monospace ]
                , padding 3
                ]
                { onPress = Just <| opts.toggleSteelShade <| Shade.toggle steelShade
                , label = text <| Shade.toString steelShade
                }
        , el [ paddingEach { edges | left = 2 } ] <|
            text (String.fromInt steelVal)
        , el [ paddingEach { edges | left = 10 } ] <|
            questionsButton opts.openSteelModal
        , el
            [ alignTop
            , alignLeft
            , transparent (steelVal > 0)
            , paddingEach { edges | left = 20 }
            ]
            Components.warningIcon
        ]


questionsButton : msg -> Element msg
questionsButton msg =
    Input.button
        [ Border.color Colors.shadow
        , Border.rounded 4
        , Border.width 1
        , padding 3
        , Font.size 12
        ]
        { onPress = Just msg
        , label = text "questions"
        }


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
