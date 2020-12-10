module Model.Lifepath exposing
    ( DnDOptions
    , Lifepath
    , Options
    , Warnings
    , decoder
    , mentionedIn
    , view
    )

import Common exposing (edges)
import Components
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath.GenSkills as GenSkills exposing (GenSkills)
import Model.Lifepath.Lead as Lead exposing (Lead)
import Model.Lifepath.Requirement as Requirement exposing (Requirement)
import Model.Lifepath.Resources as Resources exposing (Resources)
import Model.Lifepath.Skill as Skill exposing (Skill)
import Model.Lifepath.StatMod as StatMod exposing (StatMod)
import Model.Lifepath.Trait as Trait exposing (Trait)
import Model.Lifepath.Years as Years exposing (Years)
import String.Extra exposing (toTitleCase)


type alias Lifepath =
    { id : Int
    , settingId : Int
    , settingName : String
    , name : String
    , page : Int
    , years : Years
    , statMod : StatMod
    , res : Resources
    , leads : List Lead
    , genSkills : GenSkills
    , skillPts : Int
    , traitPts : Int
    , skills : List Skill
    , traits : List Trait
    , born : Bool
    , requirement : Maybe Requirement
    }


mentionedIn : Lifepath -> Requirement.Predicate -> Bool
mentionedIn lifepath predicate =
    case predicate of
        Requirement.SpecificLifepath { lifepathId } ->
            lifepathId == lifepath.id

        Requirement.PreviousLifepaths _ ->
            True

        Requirement.Setting { settingId } ->
            settingId == lifepath.settingId

        Requirement.Any preds ->
            NonEmpty.any (mentionedIn lifepath) preds

        Requirement.All preds ->
            NonEmpty.any (mentionedIn lifepath) preds


type alias Options msg =
    { dndOptions : Maybe (DnDOptions msg)
    , lifepath : Lifepath
    , id : String
    , warnings : Warnings
    , onClickRequirement : Requirement -> Int -> msg
    , onDelete : Int -> msg
    }


type alias DnDOptions msg =
    { dragStyles : List (Attribute msg)
    , dropStyles : List (Attribute msg)
    , dragIndex : Int
    }


type alias Warnings =
    { general : List String
    , requirementSatisfied : Bool
    }


view : Options msg -> Element msg
view opts =
    let
        baseAttrs =
            [ width fill
            , spacing 10
            , paddingXY 20 20
            , htmlAttribute <| Html.Attributes.id opts.id
            ]

        attrs =
            case opts.dndOptions of
                Just { dropStyles } ->
                    baseAttrs ++ dropStyles

                Nothing ->
                    baseAttrs
    in
    row attrs
        [ column [ width fill, spacing 5 ] <|
            [ row [ width fill ]
                [ case opts.dndOptions of
                    Just { dragStyles } ->
                        Components.dragHandle dragStyles

                    Nothing ->
                        none
                , text <| toTitleCase opts.lifepath.name
                , el [ Font.size 18, paddingEach { edges | left = 20 } ] <|
                    (text <| "(" ++ toTitleCase opts.lifepath.settingName ++ ")")
                , el
                    [ alignTop
                    , alignLeft
                    , Components.tooltip above (Components.warningsTooltip opts.warnings.general)
                    , transparent (List.isEmpty opts.warnings.general)
                    , paddingEach { edges | left = 20 }
                    ]
                    Components.warningIcon
                ]
            , textColumn [ width fill, Font.size 18 ]
                [ row [ width fill, spacing 10, Font.size 18 ]
                    [ text <| Years.toString opts.lifepath.years
                    , text <| Resources.toString opts.lifepath.res
                    , text <| StatMod.toString opts.lifepath.statMod
                    ]
                , paragraph [] <|
                    [ text <| "Skills: "
                    , GenSkills.toString opts.lifepath.genSkills
                        |> Maybe.map (\genText -> text (genText ++ ", "))
                        |> Maybe.withDefault none
                    , text <| String.fromInt opts.lifepath.skillPts ++ " pts: "
                    ]
                        ++ (List.intersperse (text ", ") <|
                                List.map viewSkill opts.lifepath.skills
                           )
                , paragraph [] <|
                    [ text <| "Traits: "
                    , text <|
                        String.fromInt opts.lifepath.traitPts
                            ++ " pts: "
                            ++ (String.join ", " <|
                                    List.map (Trait.name >> toTitleCase)
                                        opts.lifepath.traits
                               )
                    ]
                , paragraph [ width fill ] <|
                    [ text "Leads: "
                    , text <|
                        String.join ", " <|
                            List.map (.settingName >> toTitleCase) opts.lifepath.leads
                    ]
                , requirementRow opts
                ]
            ]
        , case opts.dndOptions of
            Just { dragIndex } ->
                deleteButton <| opts.onDelete dragIndex

            Nothing ->
                none
        ]


requirementRow : Options msg -> Element msg
requirementRow opts =
    -- this nonsense seems necessary to make the width work in both the modal and the page
    case ( opts.lifepath.requirement, opts.dndOptions ) of
        ( Nothing, _ ) ->
            none

        ( Just requirement, Nothing ) ->
            row [] <|
                [ paragraph [] [ text <| "Requires: " ++ requirement.description ]
                ]

        ( Just requirement, Just { dragIndex } ) ->
            row [] <|
                [ text <| "Requires: " ++ requirement.description
                , Input.button
                    [ alignTop
                    , alignLeft
                    , paddingEach { edges | left = 20 }
                    , Components.tooltip above <|
                        Components.warningsTooltip [ "Satisfy missing lifepath requirement" ]
                    , transparent opts.warnings.requirementSatisfied
                    ]
                    { onPress = Just <| opts.onClickRequirement requirement dragIndex
                    , label = Components.warningIcon
                    }
                ]


deleteButton : msg -> Element msg
deleteButton onPress =
    Input.button
        [ alignRight, alignTop ]
        { onPress = Just onPress
        , label = Components.deleteIcon
        }


viewSkill : Skill -> Element msg
viewSkill skill =
    let
        suffix : Maybe String
        suffix =
            case ( skill.magical, skill.training ) of
                ( False, False ) ->
                    Nothing

                ( True, False ) ->
                    Just "§"

                ( False, True ) ->
                    Just "†"

                ( True, True ) ->
                    -- NOTE this doesn't appear in book data
                    -- but we'll still want another symbol
                    -- (double dagger is taken by elves)
                    Just "§"
    in
    Element.row [] <|
        List.filterMap identity
            [ Just <| Element.text <| Skill.toString skill
            , Maybe.map Components.superScript suffix
            ]



-- DECODE


decoder : Decoder Lifepath
decoder =
    Decode.succeed Lifepath
        |> required "id" Decode.int
        |> required "settingId" Decode.int
        |> required "settingName" Decode.string
        |> required "name" Decode.string
        |> required "page" Decode.int
        |> required "years" Years.decoder
        |> required "statMod" StatMod.decoder
        |> required "resources" Resources.decoder
        |> required "leads" (Decode.list Lead.decoder)
        |> required "genSkills" GenSkills.decoder
        |> required "skillPts" Decode.int
        |> required "traitPts" Decode.int
        |> required "skillList" (Decode.list Skill.decode)
        |> required "traitList" (Decode.list Trait.decode)
        |> required "born" Decode.bool
        |> optional "requirement" (Decode.map Just Requirement.decoder) Nothing
