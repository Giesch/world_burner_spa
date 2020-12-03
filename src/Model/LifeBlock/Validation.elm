module Model.LifeBlock.Validation exposing
    ( Error(..)
    , Warning(..)
    , WarningReason(..)
    , errors
    , includes
    , reason
    , warnings
    )

import Array exposing (Array)
import Common
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath exposing (Lifepath)
import Model.Lifepath.Requirement as Requirement exposing (Requirement)
import String.Extra exposing (toTitleCase)


{-| A problem with a character that makes it immediately invalid.
ie a non-first born lifepath or a missing setting lead
-}
type Error
    = Error String


{-| A problem with a character that makes it incomplete.
ie a missing born lifepath or an unsatisfied requirement
-}
type Warning
    = Warning WarningData


type alias WarningData =
    { message : String
    , reason : WarningReason
    }


{-| Reasons for a warning that can be used to filter lifepaths for a solution
-}
type WarningReason
    = Unmet UnmetRequirements
    | MissingBorn


type alias UnmetRequirements =
    { predicates : NonEmpty Requirement.Predicate
    , lifepath : Lifepath
    }


reason : Warning -> WarningReason
reason (Warning data) =
    data.reason


{-| Takes a predicate and the PREVIOUS lifepaths of the character.
The failure result includes the predicates that could fix the failure.
-}
pass : Requirement.Predicate -> Array Lifepath -> Result (NonEmpty Requirement.Predicate) ()
pass predicate previousPaths =
    case predicate of
        Requirement.SpecificLifepath { lifepathId, count } ->
            if atLeast count (\lp -> lp.id == lifepathId) previousPaths then
                Ok ()

            else
                Err <| NonEmpty.singleton predicate

        Requirement.Setting { settingId, count } ->
            if atLeast count (\lp -> lp.settingId == settingId) previousPaths then
                Ok ()

            else
                Err <| NonEmpty.singleton predicate

        Requirement.PreviousLifepaths count ->
            if Array.length previousPaths >= count then
                Ok ()

            else
                Err <| NonEmpty.singleton predicate

        Requirement.Any predicates ->
            -- This should only check if one passed, but include all failures as possible fixes
            if NonEmpty.any (\pred -> Common.isOk <| pass pred previousPaths) predicates then
                Ok ()

            else
                Err predicates

        Requirement.All predicates ->
            -- This should check all predicates and include only the ones that failed as possible fixes
            case NonEmpty.filter (\pred -> not <| Common.isOk <| pass pred previousPaths) predicates of
                Nothing ->
                    Ok ()

                Just failures ->
                    Err failures


includes : Lifepath -> Requirement.Predicate -> Bool
includes lifepath predicate =
    case predicate of
        Requirement.SpecificLifepath { lifepathId } ->
            lifepath.id == lifepathId

        Requirement.Setting { settingId } ->
            lifepath.settingId == settingId

        Requirement.PreviousLifepaths _ ->
            True

        Requirement.Any predicates ->
            NonEmpty.any (includes lifepath) predicates

        Requirement.All predicates ->
            -- This depends on the predicates already having been filtered
            NonEmpty.any (includes lifepath) predicates


atLeast : Int -> (Lifepath -> Bool) -> Array Lifepath -> Bool
atLeast count pred lifepaths =
    lifepaths
        |> Array.filter pred
        |> Array.length
        |> (\length -> length >= count)


unmetReqs : NonEmpty Lifepath -> List Warning
unmetReqs lifepaths =
    let
        check : Lifepath -> Array Lifepath -> Requirement -> Maybe Warning
        check lifepath pastLifepaths requirement =
            case pass requirement.predicate pastLifepaths of
                Ok () ->
                    Nothing

                Err predicates ->
                    let
                        message =
                            -- TODO this message is wrong for previous lifepaths,
                            -- and its weird if there's multiple requirements
                            toTitleCase lifepath.name ++ " requires " ++ requirement.description

                        unmet =
                            Unmet { predicates = predicates, lifepath = lifepath }
                    in
                    Just <| Warning { message = message, reason = unmet }

        run : ( Array Lifepath, List Lifepath, List Warning ) -> List Warning
        run ( seen, unseen, warns ) =
            case unseen of
                [] ->
                    List.reverse warns

                lifepath :: rest ->
                    case Maybe.andThen (check lifepath seen) lifepath.requirement of
                        Nothing ->
                            run ( Array.push lifepath seen, rest, warns )

                        Just warn ->
                            run ( Array.push lifepath seen, rest, warn :: warns )
    in
    run ( Array.empty, NonEmpty.toList lifepaths, [] )


gottaBeBorn : NonEmpty Lifepath -> Maybe Warning
gottaBeBorn ( first, _ ) =
    if first.born then
        Nothing

    else
        Just <|
            Warning
                { message = "A character's first lifepath must be a 'born' lifepath"
                , reason = MissingBorn
                }


bornFirst : NonEmpty Lifepath -> NonEmpty Lifepath -> Maybe Error
bornFirst _ ( notBorn, _ ) =
    if notBorn.born then
        Just <| Error "Only a character's first lifepath may be a 'born' lifepath"

    else
        Nothing


checkLead : NonEmpty Lifepath -> NonEmpty Lifepath -> Maybe Error
checkLead first ( to, _ ) =
    let
        from : Lifepath
        from =
            NonEmpty.last first

        leads : List Int
        leads =
            List.map .settingId from.leads
    in
    if to.settingId == from.settingId || List.member to.settingId leads then
        Nothing

    else
        Just <| Error <| from.name ++ " has no lead to " ++ to.settingName


brokenReqs : NonEmpty Lifepath -> NonEmpty Lifepath -> List Error
brokenReqs (( firstPath, _ ) as first) second =
    if firstPath.born then
        NonEmpty.append first second
            |> unmetReqs
            |> List.map warningToError

    else
        []


warningToError : Warning -> Error
warningToError (Warning { message }) =
    Error message


{-| Returns errors when combining two valid blocks
-}
errors : NonEmpty Lifepath -> NonEmpty Lifepath -> List Error
errors first second =
    List.filterMap (\rule -> rule first second)
        [ bornFirst
        , checkLead
        ]
        ++ brokenReqs first second


warnings : NonEmpty Lifepath -> List Warning
warnings lifepaths =
    case gottaBeBorn lifepaths of
        Just bornWarning ->
            bornWarning :: unmetReqs lifepaths

        Nothing ->
            unmetReqs lifepaths
