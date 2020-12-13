module Model.Lifepath.Validation exposing
    ( PathWithWarnings
    , ValidatedLifepaths
    , addWarnings
    , emptyWarnings
    , unpack
    )

import Common
import Dict exposing (Dict)
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Lifepath.Requirement as Requirement exposing (Predicate, Requirement)


type ValidatedLifepaths
    = Validated (NonEmpty PathWithWarnings)


unpack : ValidatedLifepaths -> NonEmpty PathWithWarnings
unpack (Validated paths) =
    paths


type alias PathWithWarnings =
    { lifepath : Lifepath
    , warnings : Lifepath.Warnings
    }


emptyWarnings : Lifepath.Warnings
emptyWarnings =
    { general = []
    , hasLead = True
    , requirementSatisfied = True
    }


addWarnings : NonEmpty Lifepath -> ValidatedLifepaths
addWarnings lifepaths =
    let
        initialPairs : NonEmpty PathWithWarnings
        initialPairs =
            NonEmpty.map withEmptyWarnings lifepaths
    in
    initialPairs
        |> addMissingBornWarning
        |> addMisplacedBornWarning
        |> checkRequirements
        |> checkLeads
        |> Validated


withEmptyWarnings : Lifepath -> PathWithWarnings
withEmptyWarnings lifepath =
    { lifepath = lifepath, warnings = emptyWarnings }


addMissingBornWarning : NonEmpty PathWithWarnings -> NonEmpty PathWithWarnings
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


addMisplacedBornWarning : NonEmpty PathWithWarnings -> NonEmpty PathWithWarnings
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


checkRequirements : NonEmpty PathWithWarnings -> NonEmpty PathWithWarnings
checkRequirements ( first, rest ) =
    List.foldl checkPath (initialSummary first) rest
        |> .validated


checkPath : PathWithWarnings -> Summary -> Summary
checkPath pair data =
    case pair.lifepath.requirement of
        Nothing ->
            addLifepath pair data

        Just requirement ->
            if satisfies data requirement.predicate then
                addLifepath pair data

            else
                addLifepathWithWarning pair data


satisfies : Summary -> Predicate -> Bool
satisfies data pred =
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
            NonEmpty.any (satisfies data) preds

        Requirement.All preds ->
            NonEmpty.all (satisfies data) preds


addLifepathWithWarning : PathWithWarnings -> Summary -> Summary
addLifepathWithWarning { lifepath, warnings } =
    addLifepath
        { lifepath = lifepath
        , warnings = { warnings | requirementSatisfied = False }
        }


addLifepath : PathWithWarnings -> Summary -> Summary
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
    , validated : NonEmpty PathWithWarnings
    }


initialSummary : PathWithWarnings -> Summary
initialSummary first =
    { lifepathIdCounts = Dict.fromList [ ( first.lifepath.id, 1 ) ]
    , settingIdCounts = Dict.fromList [ ( first.lifepath.settingId, 1 ) ]
    , totalLifepaths = 1
    , validated = NonEmpty.singleton <| checkFirstRequirement first
    }


checkFirstRequirement : PathWithWarnings -> PathWithWarnings
checkFirstRequirement { lifepath, warnings } =
    let
        satisfied =
            case lifepath.requirement of
                Just _ ->
                    False

                Nothing ->
                    True
    in
    { lifepath = lifepath
    , warnings =
        { emptyWarnings
            | general = warnings.general
            , requirementSatisfied = satisfied
        }
    }


checkLeads : NonEmpty PathWithWarnings -> NonEmpty PathWithWarnings
checkLeads ( first, rest ) =
    let
        initial : NonEmpty PathWithWarnings
        initial =
            NonEmpty.singleton <| leadWarning first <| List.head rest
    in
    List.foldl checkLeadPair initial <| Common.overlappingPairs rest


checkLeadPair :
    ( PathWithWarnings, Maybe PathWithWarnings )
    -> NonEmpty PathWithWarnings
    -> NonEmpty PathWithWarnings
checkLeadPair ( current, next ) paths =
    NonEmpty.append paths <| NonEmpty.singleton <| leadWarning current next


leadWarning : PathWithWarnings -> Maybe PathWithWarnings -> PathWithWarnings
leadWarning current maybeNext =
    let
        currentWarnings =
            current.warnings
    in
    case maybeNext of
        Just next ->
            if leadSatisfied current.lifepath next.lifepath then
                current

            else
                { current | warnings = { currentWarnings | hasLead = False } }

        Nothing ->
            current


leadSatisfied : Lifepath -> Lifepath -> Bool
leadSatisfied current next =
    (current.settingId == next.settingId)
        || (List.member next.settingId <| List.map .settingId current.leads)
