module Model.Lifepath.Validation exposing
    ( ValidPathList
    , ValidatedLifepath
    , Warnings
    , emptyWarnings
    , revalidate
    , unpack
    , validate
    )

import Dict exposing (Dict)
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath exposing (Lifepath)
import Model.Lifepath.Requirement as Requirement exposing (Predicate, Requirement)


type ValidPathList
    = Validated (List ValidatedLifepath)


unpack : ValidPathList -> List ValidatedLifepath
unpack (Validated paths) =
    paths


type alias ValidatedLifepath =
    { lifepath : Lifepath
    , warnings : Warnings
    }


type alias Warnings =
    { general : List String
    , requirementSatisfied : Bool
    }


emptyWarnings : Warnings
emptyWarnings =
    { general = []
    , requirementSatisfied = True
    }


revalidate : List ValidatedLifepath -> ValidPathList
revalidate validations =
    validate <| List.map .lifepath validations


validate : List Lifepath -> ValidPathList
validate lifepaths =
    let
        initialPairs : List ValidatedLifepath
        initialPairs =
            List.map2 ValidatedLifepath lifepaths <|
                List.repeat (List.length lifepaths) emptyWarnings
    in
    initialPairs
        |> addMissingBornWarning
        |> addMisplacedBornWarning
        |> checkRequirements
        |> Validated


addMissingBornWarning : List ValidatedLifepath -> List ValidatedLifepath
addMissingBornWarning lifepathWarnings =
    let
        message =
            "A character's first lifepath must be a Born lifepath"
    in
    case lifepathWarnings of
        { lifepath, warnings } :: rest ->
            if lifepath.born then
                lifepathWarnings

            else
                { lifepath = lifepath
                , warnings = addGeneralWarning message warnings
                }
                    :: rest

        _ ->
            lifepathWarnings


addGeneralWarning : String -> Warnings -> Warnings
addGeneralWarning message warnings =
    { warnings | general = message :: warnings.general }


addMisplacedBornWarning : List ValidatedLifepath -> List ValidatedLifepath
addMisplacedBornWarning lifepathWarnings =
    let
        message =
            "Only a character's first lifepath can be a born lifepath"

        requireNonBorn { lifepath, warnings } =
            if lifepath.born then
                { lifepath = lifepath, warnings = addGeneralWarning message warnings }

            else
                { lifepath = lifepath, warnings = warnings }
    in
    case lifepathWarnings of
        first :: rest ->
            first :: List.map requireNonBorn rest

        _ ->
            lifepathWarnings


checkRequirements : List ValidatedLifepath -> List ValidatedLifepath
checkRequirements lifepaths =
    List.foldl checkPath initial lifepaths
        |> (.validated >> List.reverse)


checkPath : ValidatedLifepath -> Summary -> Summary
checkPath pair data =
    case pair.lifepath.requirement of
        Nothing ->
            addLifepath pair data

        Just requirement ->
            if satisfies requirement.predicate data then
                addLifepath pair data

            else
                addLifepathWithWarning pair data


satisfies : Predicate -> Summary -> Bool
satisfies pred data =
    let
        hasAtLeast count id dict =
            Dict.get id dict
                |> Maybe.map (\n -> n >= count)
                |> Maybe.withDefault False
    in
    case pred of
        Requirement.PreviousLifepaths count ->
            data.totalLifepaths >= count

        Requirement.SpecificLifepath { lifepathId, count } ->
            hasAtLeast count lifepathId data.lifepathIdCounts

        Requirement.Setting { settingId, count } ->
            hasAtLeast count settingId data.settingIdCounts

        Requirement.Any preds ->
            NonEmpty.any (\p -> satisfies p data) preds

        Requirement.All preds ->
            NonEmpty.all (\p -> satisfies p data) preds


addLifepathWithWarning : ValidatedLifepath -> Summary -> Summary
addLifepathWithWarning { lifepath, warnings } =
    addLifepath
        { lifepath = lifepath
        , warnings = { warnings | requirementSatisfied = False }
        }


addLifepath : ValidatedLifepath -> Summary -> Summary
addLifepath pair data =
    { lifepathIdCounts =
        Dict.update pair.lifepath.id increment data.lifepathIdCounts
    , settingIdCounts =
        Dict.update pair.lifepath.settingId increment data.settingIdCounts
    , totalLifepaths = 1 + data.totalLifepaths
    , validated = pair :: data.validated
    }


increment : Maybe Int -> Maybe Int
increment entry =
    case entry of
        Just count ->
            Just (count + 1)

        Nothing ->
            Just 1


type alias Summary =
    { lifepathIdCounts : Dict Int Int
    , settingIdCounts : Dict Int Int
    , totalLifepaths : Int
    , validated : List ValidatedLifepath
    }


initial : Summary
initial =
    { lifepathIdCounts = Dict.empty
    , settingIdCounts = Dict.empty
    , totalLifepaths = 0
    , validated = []
    }
