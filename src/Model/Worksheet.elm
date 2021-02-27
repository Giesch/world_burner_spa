module Model.Worksheet exposing
    ( Worksheet
    , age
    , changeStat
    , distributeStats
    , healthAnswers
    , lifepaths
    , new
    , replaceLifepaths
    , shadedStats
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
import Model.Stock as Stock exposing (Stock)
import Model.Worksheet.Health as Health
import Model.Worksheet.Shade as Shade exposing (Shade)
import Model.Worksheet.ShadedStats as ShadedStats exposing (ShadedStats)
import Model.Worksheet.Stat as Stat exposing (Stat)
import Model.Worksheet.Steel as Steel


type Worksheet
    = Worksheet WorksheetData


type alias WorksheetData =
    { ageRanges : List Stock.AgeRange
    , lifepaths : ValidatedLifepaths
    , age : Int
    , statsRemaining : ( StatMod.Bonus, StatMod.Bonus )
    , stats : ShadedStats
    , healthAndSteelAnswers : HealthAndSteelAnswers
    , graySteel : Bool
    }


age : Worksheet -> Int
age (Worksheet sheet) =
    sheet.age


shadedStats : Worksheet -> ShadedStats
shadedStats (Worksheet sheet) =
    sheet.stats


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


healthAnswers : Worksheet -> Health.Answers
healthAnswers (Worksheet { healthAndSteelAnswers }) =
    justHealthAnswers healthAndSteelAnswers


justHealthAnswers : HealthAndSteelAnswers -> Health.Answers
justHealthAnswers answers =
    { squalor = answers.squalor
    , frail = answers.frail
    , wounded = answers.wounded
    , enslaved = answers.enslaved
    , immortal = answers.immortal
    , athletic = answers.athletic
    , soundOfMusic = answers.soundOfMusic
    }


steelAnswers : Worksheet -> Steel.Answers
steelAnswers (Worksheet { healthAndSteelAnswers }) =
    justSteelAnswers healthAndSteelAnswers


justSteelAnswers : HealthAndSteelAnswers -> Steel.Answers
justSteelAnswers answers =
    { soldier = answers.soldier
    , wounded = answers.wounded
    , killer = answers.killer
    , enslaved = answers.enslaved
    , sheltered = answers.sheltered
    , competitiveCulture = answers.competitiveCulture
    , givenBirth = answers.givenBirth
    , gifted = answers.gifted
    }


updateSteelAnswers : Steel.Answers -> Worksheet -> Worksheet
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


updateHealthAnswers : Health.Answers -> Worksheet -> Worksheet
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


zeroStats : ShadedStats
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
            recalculateLifepathData sheet.ageRanges sheet.stats paths
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


distributeMental : Int -> ShadedStats -> ShadedStats
distributeMental points current =
    if points > 0 then
        if current.will.value <= current.perception.value then
            distributeMental (points - 1)
                { current | will = ShadedStats.updateStat current.will (current.will.value + 1) }

        else
            distributeMental (points - 1)
                { current
                    | perception =
                        ShadedStats.updateStat current.perception
                            (current.perception.value + 1)
                }

    else
        current


distributePhysical : Int -> ShadedStats -> ShadedStats
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
                { current | power = ShadedStats.updateStat current.power (current.power.value + 1) }

        else if current.forte.value == lowest then
            distributePhysical (points - 1)
                { current | forte = ShadedStats.updateStat current.forte (current.forte.value + 1) }

        else if current.agility.value == lowest then
            distributePhysical (points - 1)
                { current | agility = ShadedStats.updateStat current.agility (current.agility.value + 1) }

        else if current.speed.value == lowest then
            distributePhysical (points - 1)
                { current | speed = ShadedStats.updateStat current.speed (current.speed.value + 1) }

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

        updatedStats : ShadedStats
        updatedStats =
            ShadedStats.updateStatVal stat clampedVal sheet.stats

        updatedRemaining : ( StatMod.Bonus, StatMod.Bonus )
        updatedRemaining =
            recalculateSpentStats updatedStats <| Tuple.second sheet.statsRemaining
    in
    Worksheet
        { sheet
            | stats = updatedStats
            , statsRemaining = updatedRemaining
        }


recalculateSpentStats : ShadedStats -> StatMod.Bonus -> ( StatMod.Bonus, StatMod.Bonus )
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


new : List Stock.AgeRange -> NonEmpty Lifepath -> Worksheet
new ranges paths =
    Worksheet <| newData ranges paths


newData : List Stock.AgeRange -> NonEmpty Lifepath -> WorksheetData
newData ranges paths =
    let
        lifepathData =
            recalculateLifepathData ranges allOnes paths
    in
    { ageRanges = ranges
    , lifepaths = lifepathData.lifepaths
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


recalculateLifepathData :
    List Stock.AgeRange
    -> ShadedStats
    -> NonEmpty Lifepath
    -> LifepathData
recalculateLifepathData ranges currentStats paths =
    let
        newAge : Int
        newAge =
            sumAge paths

        totalStats : StatMod.Bonus
        totalStats =
            StatMod.addBonus (ageStats ranges newAge) (lifepathBonuses paths)

        recalculatedStatsRemaining : ( StatMod.Bonus, StatMod.Bonus )
        recalculatedStatsRemaining =
            recalculateSpentStats currentStats totalStats
    in
    { lifepaths = Validation.addWarnings paths
    , age = newAge
    , statsRemaining = recalculatedStatsRemaining
    }


allOnes : ShadedStats
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


ageStats : List Stock.AgeRange -> Int -> StatMod.Bonus
ageStats ranges a =
    let
        inRange : Stock.AgeRange -> Bool
        inRange { minAge, maxAge } =
            minAge <= a && maxAge >= a

        ageRowToBonus : Stock.AgeRange -> StatMod.Bonus
        ageRowToBonus { physical, mental } =
            { physical = physical, mental = mental, either = 0 }
    in
    ranges
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
                |> ShadedStats.get stat
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

        updatedStats : ShadedStats
        updatedStats =
            ShadedStats.updateStatShade stat newShade sheet.stats
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
    Health.compute stats (justHealthAnswers healthAndSteelAnswers)


steel : WorksheetData -> ( Shade, Int )
steel { stats, healthAndSteelAnswers, graySteel } =
    let
        answers =
            justSteelAnswers healthAndSteelAnswers

        val =
            Steel.value stats answers
    in
    if graySteel then
        ( Shade.Gray, val - 5 )

    else
        ( Shade.Black, val )


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
    , toggleStatShade : Stat -> msg
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
                    ShadedStats.get stat sheet.stats
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

        mainColumnAttrs : List (Attribute msg)
        mainColumnAttrs =
            [ width fill
            , alignTop
            , Font.size 18
            , padding 20
            , spacing 10
            ]
    in
    [ el [ padding 20 ] <|
        Components.faintButton "Distribute" opts.distributeStats
    , row []
        [ column mainColumnAttrs
            [ text "Stats:"
            , table [ spacing 5 ]
                { data = statRows
                , columns =
                    [ { header = none
                      , width = shrink
                      , view =
                            \{ stat } ->
                                el [ alignBottom, paddingEach { edges | right = 10 } ] <|
                                    (text <| Stat.toString stat)
                      }
                    , { header = none
                      , width = shrink
                      , view = toggleShadeButton (.stat >> opts.toggleStatShade)
                      }
                    , { header = none
                      , width = shrink |> minimum 25
                      , view =
                            \{ value, shade } ->
                                el [ alignBottom ] <|
                                    (text <| Shade.toString shade ++ String.fromInt value)
                      }
                    , { header = none
                      , width = shrink
                      , view = changeStatButtons opts.changeStat
                      }
                    ]
                }
            ]
        , column mainColumnAttrs
            [ text "Remaining:"
            , table
                [ spacing 5
                , width (shrink |> minimum 200)
                ]
                { data =
                    [ { name = "Mental", prop = .mental, warn = True }
                    , { name = "Physical", prop = .physical, warn = True }
                    , { name = "Either", prop = .either, warn = False }
                    ]
                , columns =
                    [ { header = none
                      , width = shrink
                      , view =
                            \{ name } ->
                                text <| name ++ ":"
                      }
                    , { header = none
                      , width = shrink
                      , view =
                            \{ prop } ->
                                text <| viewRemaining prop
                      }
                    , { header = none
                      , width = shrink
                      , view =
                            \{ warn, prop } ->
                                if warn then
                                    statWarning prop

                                else
                                    none
                      }
                    ]
                }
            ]
        , column mainColumnAttrs <|
            let
                viewShadedVal : ( Shade, Int ) -> Element msg
                viewShadedVal ( shade, val ) =
                    text <| Shade.toString shade ++ String.fromInt val

                steelName =
                    "Steel"
            in
            [ text "Attributes:"
            , table [ spacing 5 ]
                { data =
                    [ { name = "Mortal Wound"
                      , val = viewShadedVal <| mortalWound sheet
                      , questions = Nothing
                      , warning = False
                      }
                    , { name = "Reflexes"
                      , val = viewShadedVal <| reflexes sheet
                      , questions = Nothing
                      , warning = False
                      }
                    , { name = "Health"
                      , val = viewShadedVal <| health sheet
                      , questions = Just opts.openHealthModal
                      , warning = False
                      }
                    , let
                        shadedSteel =
                            steel sheet
                      in
                      { name = steelName
                      , val = viewShadedVal shadedSteel
                      , questions = Just opts.openSteelModal
                      , warning = Tuple.second shadedSteel < 0
                      }
                    , { name = "Hesitation"
                      , val = text <| String.fromInt <| hesitation sheet
                      , questions = Nothing
                      , warning = False
                      }
                    , { name = "Stride"
                      , val = text <| String.fromInt <| stride sheet
                      , questions = Nothing
                      , warning = False
                      }
                    , { name = "Circles"
                      , val = viewShadedVal <| circles sheet
                      , questions = Nothing
                      , warning = False
                      }
                    , { name = "Resources"
                      , val = viewShadedVal <| resources sheet
                      , questions = Nothing
                      , warning = False
                      }
                    ]
                , columns =
                    [ { header = none
                      , width = shrink
                      , view =
                            \{ name, warning } ->
                                let
                                    icon =
                                        if warning then
                                            el [ alignRight ] <| Components.warningIcon

                                        else
                                            none
                                in
                                el [ paddingEach { edges | right = 5 } ] <|
                                    row [ width fill ] [ text <| name ++ ":", icon ]
                      }
                    , { header = none
                      , width = shrink |> minimum 40
                      , view = .val
                      }
                    , { header = none
                      , width = shrink
                      , view =
                            \{ questions } ->
                                Maybe.map questionsButton questions
                                    |> Maybe.withDefault none
                      }
                    , { header = none
                      , width = shrink
                      , view =
                            \row ->
                                if row.name == steelName then
                                    toggleShadeButton
                                        (\_ -> opts.toggleSteelShade <| Shade.toggle <| Tuple.first <| steel sheet)
                                        row

                                else
                                    none
                      }
                    ]
                }
            ]
        ]
    ]


type alias StatRow =
    { stat : Stat
    , value : Int
    , shade : Shade
    }


textButton : msg -> String -> Element msg
textButton msg label =
    Input.button
        [ Border.color Colors.shadow
        , Border.rounded 4
        , Border.width 1
        , padding 3
        , Font.size 12
        , centerX
        , centerY
        ]
        { onPress = Just msg
        , label = el [ centerX, centerY ] <| text label
        }


questionsButton : msg -> Element msg
questionsButton msg =
    textButton msg "questions"


toggleShadeButton : (row -> msg) -> row -> Element msg
toggleShadeButton toggle row =
    textButton (toggle row) "shade"


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
