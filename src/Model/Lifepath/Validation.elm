module Model.Lifepath.Validation exposing
    ( ValidatedLifepath
    , revalidate
    , validate
    )

import Model.Lifepath as Lifepath exposing (Lifepath)


type alias ValidatedLifepath =
    ( Lifepath, List String )


revalidate : List ValidatedLifepath -> List ValidatedLifepath
revalidate validations =
    validate <| List.map Tuple.first validations


validate : List Lifepath -> List ValidatedLifepath
validate lifepaths =
    let
        initialPairs : List ( Lifepath, List String )
        initialPairs =
            List.map2 Tuple.pair lifepaths <|
                List.repeat (List.length lifepaths) []
    in
    initialPairs
        |> addMissingBornWarning
        |> addMisplacedBornWarning


addMissingBornWarning : List ValidatedLifepath -> List ValidatedLifepath
addMissingBornWarning lifepathWarnings =
    case lifepathWarnings of
        ( firstPath, firstWarns ) :: rest ->
            if firstPath.born then
                lifepathWarnings

            else
                ( firstPath
                , "A character's first lifepath must be a Born lifepath" :: firstWarns
                )
                    :: rest

        _ ->
            lifepathWarnings


addMisplacedBornWarning : List ValidatedLifepath -> List ValidatedLifepath
addMisplacedBornWarning lifepathWarnings =
    case lifepathWarnings of
        first :: rest ->
            first :: List.map requireNotBorn rest

        _ ->
            lifepathWarnings


requireNotBorn : ValidatedLifepath -> ValidatedLifepath
requireNotBorn ( lifepath, warnings ) =
    if lifepath.born then
        ( lifepath, "Only a character's first lifepath can be a born lifepath" :: warnings )

    else
        ( lifepath, warnings )
