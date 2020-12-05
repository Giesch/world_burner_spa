module Model.Lifepath exposing
    ( Lifepath
    , decoder
    )

import Colors exposing (..)
import Element exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
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
    , searchContent : List String
    }


type alias LifepathJson =
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



-- DECODE


decoder : Decoder Lifepath
decoder =
    Decode.succeed LifepathJson
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
        |> Decode.map addSearchContent


addSearchContent : LifepathJson -> Lifepath
addSearchContent json =
    { id = json.id
    , settingId = json.settingId
    , settingName = json.settingName
    , name = json.name
    , page = json.page
    , years = json.years
    , statMod = json.statMod
    , res = json.res
    , leads = json.leads
    , genSkills = json.genSkills
    , skillPts = json.skillPts
    , traitPts = json.traitPts
    , skills = json.skills
    , traits = json.traits
    , born = json.born
    , requirement = json.requirement
    , searchContent = searchContent json
    }


searchContent : LifepathJson -> List String
searchContent json =
    let
        skills : List String
        skills =
            List.map .displayName json.skills

        traits : List String
        traits =
            List.map Trait.name json.traits
    in
    json.name :: json.settingName :: skills ++ traits
