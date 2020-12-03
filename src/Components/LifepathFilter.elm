module Components.LifepathFilter exposing
    ( LifepathFilter
    , LifepathFilterOptions
    , apply
    , none
    , view
    , withFit
    , withFix
    , withSearchTerm
    )

import Array exposing (Array)
import Colors
import Common
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.LifeBlock as LifeBlock
import Model.LifeBlock.Validation as Validation
import Model.Lifepath exposing (Lifepath)


type alias LifepathFilter =
    { searchTerm : String
    , fit : Maybe LifeBlock.Fit
    , fix : Maybe (NonEmpty Validation.WarningReason)
    }


none : LifepathFilter
none =
    { searchTerm = ""
    , fit = Nothing
    , fix = Nothing
    }


withSearchTerm : String -> LifepathFilter -> LifepathFilter
withSearchTerm searchTerm filter =
    { filter | searchTerm = searchTerm }


withFit : Maybe LifeBlock.Fit -> LifepathFilter -> LifepathFilter
withFit fit filter =
    { filter | fit = fit }


withFix : Maybe (NonEmpty Validation.WarningReason) -> LifepathFilter -> LifepathFilter
withFix fix filter =
    { filter | fix = fix }


apply : LifepathFilter -> Array Lifepath -> Array Lifepath
apply filter lifepaths =
    Array.filter (include filter) lifepaths


include : LifepathFilter -> Lifepath -> Bool
include filter lifepath =
    includedByTerm filter lifepath
        && includedByFit filter lifepath
        && includedByFix filter lifepath


includedByTerm : LifepathFilter -> Lifepath -> Bool
includedByTerm filter lifepath =
    case String.toLower filter.searchTerm of
        "" ->
            True

        term ->
            List.any
                (\field -> String.contains term field)
                lifepath.searchContent


includedByFit : LifepathFilter -> Lifepath -> Bool
includedByFit filter lifepath =
    case filter.fit of
        Nothing ->
            True

        Just ( LifeBlock.Before, lifeBlock ) ->
            LifeBlock.combine (LifeBlock.singleton lifepath) lifeBlock
                |> Common.isOk

        Just ( LifeBlock.After, lifeBlock ) ->
            LifeBlock.combine lifeBlock (LifeBlock.singleton lifepath)
                |> Common.isOk


includedByFix : LifepathFilter -> Lifepath -> Bool
includedByFix filter lifepath =
    case filter.fix of
        Nothing ->
            True

        Just reasons ->
            NonEmpty.any
                (\reason -> includedByWarningReason reason lifepath)
                reasons


includedByWarningReason : Validation.WarningReason -> Lifepath -> Bool
includedByWarningReason reason lifepath =
    case reason of
        Validation.MissingBorn ->
            lifepath.born

        Validation.Unmet { predicates } ->
            NonEmpty.any (Validation.includes lifepath) predicates


type alias LifepathFilterOptions msg =
    { enteredSearchText : String -> msg
    , clearFit : msg
    , clearFix : msg
    }


view : LifepathFilterOptions msg -> LifepathFilter -> Element msg
view { enteredSearchText, clearFit, clearFix } { searchTerm, fit, fix } =
    column [ alignRight, padding 40, width fill ]
        [ fitFilters { fit = fit, clearFit = clearFit }
        , fixFilters { fix = fix, clearFix = clearFix }
        , searchInput enteredSearchText <| searchTerm
        ]


type alias FixOptions msg =
    { fix : Maybe (NonEmpty Validation.WarningReason)
    , clearFix : msg
    }


fixFilters : FixOptions msg -> Element msg
fixFilters { fix, clearFix } =
    case fix of
        Nothing ->
            Element.none

        Just reasons ->
            let
                labels : List String
                labels =
                    NonEmpty.toList <| NonEmpty.map label reasons

                label : Validation.WarningReason -> String
                label reason =
                    case reason of
                        Validation.MissingBorn ->
                            "born lifepath"

                        Validation.Unmet { lifepath } ->
                            "required by " ++ lifepath.name
            in
            row [ width fill ]
                [ Input.button [ width <| fillPortion 1 ]
                    { onPress = Just clearFix, label = text "X" }
                , column [ width <| fillPortion 2 ] <| List.map text ("filtered:" :: labels)
                ]


type alias FitOptions msg =
    { fit : Maybe LifeBlock.Fit
    , clearFit : msg
    }


fitFilters : FitOptions msg -> Element msg
fitFilters { fit, clearFit } =
    case fit of
        Nothing ->
            Element.none

        Just f ->
            viewFit clearFit f


viewFit : msg -> LifeBlock.Fit -> Element msg
viewFit clearFit ( position, block ) =
    let
        label =
            case position of
                LifeBlock.Before ->
                    "filter: fits before:"

                LifeBlock.After ->
                    "filter: fits after:"

        pathNames : List String
        pathNames =
            block
                |> LifeBlock.paths
                |> NonEmpty.toList
                |> List.map .name
    in
    row [ width fill ]
        [ Input.button [ width <| fillPortion 1 ]
            { onPress = Just clearFit
            , label = text "X"
            }
        , column [ width <| fillPortion 2 ]
            (text label :: List.map text pathNames)
        ]


searchInput : (String -> msg) -> String -> Element msg
searchInput enteredSearchText searchTerm =
    Input.search [ Font.color Colors.black ]
        { onChange = enteredSearchText
        , text = searchTerm
        , placeholder = Nothing
        , label = Input.labelAbove [] <| text "Search"
        }
