module Model.Lifepath exposing
    ( Lifepath
    , decoder
    , mentionedIn
    )

import Colors exposing (..)
import Element exposing (..)
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
