module Model.Lifepath.Validation exposing
    ( ValidPathList
    , ValidatedLifepath
    , emptyWarnings
    , revalidate
    , unpack
    , validate
    )

import Dict exposing (Dict)
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Lifepath.Requirement as Requirement exposing (Predicate, Requirement)


type ValidPathList
    = Validated (NonEmpty ValidatedLifepath)


unpack : ValidPathList -> NonEmpty ValidatedLifepath
unpack (Validated paths) =
    paths


type alias ValidatedLifepath =
    { lifepath : Lifepath
    , warnings : Lifepath.Warnings
    }


emptyWarnings : Lifepath.Warnings
emptyWarnings =
    { general = []
    , requirementSatisfied = True
    }


revalidate : NonEmpty ValidatedLifepath -> ValidPathList
revalidate validations =
    validate <| NonEmpty.map .lifepath validations


validate : NonEmpty Lifepath -> ValidPathList
validate lifepaths =
    let
        initialPairs : NonEmpty ValidatedLifepath
        initialPairs =
            NonEmpty.map withEmptyWarnings lifepaths
    in
    initialPairs
        |> addMissingBornWarning
        |> addMisplacedBornWarning
        |> checkRequirements
        |> Validated


withEmptyWarnings : Lifepath -> ValidatedLifepath
withEmptyWarnings lifepath =
    { lifepath = lifepath, warnings = emptyWarnings }


addMissingBornWarning : NonEmpty ValidatedLifepath -> NonEmpty ValidatedLifepath
addMissingBornWarning (( { lifepath, warnings }, rest ) as lifepathWarnings) =
    let
        message =
            "A character's first lifepath must be a Born lifepath"
    in
    if lifepath.born then
        lifepathWarnings

    else
        ( { lifepath = lifepath
          , warnings = addGeneralWarning message warnings
          }
        , rest
        )


addGeneralWarning : String -> Lifepath.Warnings -> Lifepath.Warnings
addGeneralWarning message warnings =
    { warnings | general = message :: warnings.general }


addMisplacedBornWarning : NonEmpty ValidatedLifepath -> NonEmpty ValidatedLifepath
addMisplacedBornWarning ( first, rest ) =
    let
        message =
            "Only a character's first lifepath can be a born lifepath"

        requireNonBorn { lifepath, warnings } =
            if lifepath.born then
                { lifepath = lifepath, warnings = addGeneralWarning message warnings }

            else
                { lifepath = lifepath, warnings = warnings }
    in
    ( first, List.map requireNonBorn rest )


checkRequirements : NonEmpty ValidatedLifepath -> NonEmpty ValidatedLifepath
checkRequirements ( first, rest ) =
    List.foldl checkPath (initialSummary first) rest
        |> .validated


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
    , validated = NonEmpty.append data.validated (NonEmpty.singleton pair)
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
    , validated : NonEmpty ValidatedLifepath
    }


initialSummary : ValidatedLifepath -> Summary
initialSummary first =
    { lifepathIdCounts = Dict.empty
    , settingIdCounts = Dict.empty
    , totalLifepaths = 0
    , validated = NonEmpty.singleton <| markFirstRequirement first
    }


markFirstRequirement : ValidatedLifepath -> ValidatedLifepath
markFirstRequirement { lifepath, warnings } =
    { lifepath = lifepath
    , warnings =
        { general = warnings.general
        , requirementSatisfied = False
        }
    }
