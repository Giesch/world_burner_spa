module Model.Worksheet exposing
    ( Worksheet
    , lifepaths
    , new
    , replaceLifepaths
    , updateLifepaths
    )

{-| A module for the data that the user can edit after choosing lifepaths.
-}

import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Lifepath.Validation as Validation exposing (ValidPathList, ValidatedLifepath)


type Worksheet
    = Worksheet
        { lifepaths : ValidPathList
        }


lifepaths : Worksheet -> List Validation.ValidatedLifepath
lifepaths (Worksheet sheet) =
    sheet.lifepaths
        |> Validation.unpack
        |> NonEmpty.toList


updateLifepaths :
    (List ValidatedLifepath -> List ValidatedLifepath)
    -> Worksheet
    -> Maybe Worksheet
updateLifepaths update workSheet =
    workSheet
        |> lifepaths
        |> update
        |> (\paths -> replaceLifepaths paths workSheet)


replaceLifepaths : List ValidatedLifepath -> Worksheet -> Maybe Worksheet
replaceLifepaths paths _ =
    paths
        |> List.map .lifepath
        |> NonEmpty.fromList
        |> Maybe.map new


new : NonEmpty Lifepath -> Worksheet
new paths =
    Worksheet { lifepaths = Validation.validate paths }
