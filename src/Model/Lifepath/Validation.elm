module Model.Lifepath.Validation exposing (validate)

import Model.Lifepath as Lifepath exposing (Lifepath)


validate : List Lifepath -> List ( Lifepath, List String )
validate lifepaths =
    let
        initialPairs : List ( Lifepath, List String )
        initialPairs =
            List.map2 Tuple.pair lifepaths <|
                List.repeat (List.length lifepaths) []
    in
    addBornWarning initialPairs


addBornWarning : List ( Lifepath, List String ) -> List ( Lifepath, List String )
addBornWarning lifepathWarnings =
    case lifepathWarnings of
        ( firstPath, firstWarns ) :: rest ->
            if firstPath.born then
                lifepathWarnings

            else
                ( firstPath, "A character's first lifepath must be a Born lifepath" :: firstWarns )
                    :: rest

        _ ->
            lifepathWarnings
