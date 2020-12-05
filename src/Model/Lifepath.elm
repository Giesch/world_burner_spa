module Model.Lifepath exposing
    ( Lifepath
    , decoder
    , lifepathWidth
    , view
    )

import Colors exposing (..)
import Common
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
import String.Extra exposing (toTitleCase)


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



-- VIEW


lifepathWidth : Element.Length
lifepathWidth =
    Element.px 300


view : Lifepath -> Element msg
view lifepath =
    let
        defaultAttrs : List (Attribute msg)
        defaultAttrs =
            [ Background.color Colors.white
            , Font.color Colors.black
            , Border.rounded 8
            , Border.color Colors.darkened
            , Border.width 1
            , padding 12
            , width lifepathWidth
            , spacing 10
            ]
                ++ Common.userSelectNone

        attrs =
            defaultAttrs
    in
    column attrs
        [ text <| toTitleCase lifepath.name ++ " (" ++ toTitleCase lifepath.settingName ++ ")"
        , row [ width fill, spaceEvenly ]
            [ text <| Years.toString lifepath.years
            , text <| Resources.toString lifepath.res
            , text <| StatMod.toString lifepath.statMod
            ]
        , viewSkills lifepath.skillPts lifepath.skills
        , viewTraits lifepath.traitPts lifepath.traits
        , viewLeads lifepath.leads
        ]


viewSkills : Int -> List Skill -> Element msg
viewSkills pts skills =
    let
        skillNames =
            String.join ", " <| List.map (\sk -> toTitleCase <| .displayName sk) skills
    in
    case ( pts, List.length skills ) of
        ( 0, 0 ) ->
            none

        ( _, 0 ) ->
            paragraph []
                [ text ("Skills: " ++ String.fromInt pts) ]

        _ ->
            paragraph []
                [ text ("Skills: " ++ String.fromInt pts ++ ": " ++ skillNames) ]


viewLeads : List Lead -> Element msg
viewLeads leads =
    let
        leadNames =
            String.join ", " <|
                List.map (\lead -> toTitleCase <| .settingName lead) leads
    in
    if List.length leads == 0 then
        none

    else
        paragraph []
            [ text <| "Leads: " ++ leadNames ]


viewTraits : Int -> List Trait -> Element msg
viewTraits pts traits =
    let
        traitNames =
            String.join ", " <| List.map (Trait.name >> toTitleCase) traits
    in
    case ( List.isEmpty traits, pts ) of
        ( True, 0 ) ->
            none

        ( True, _ ) ->
            paragraph []
                [ text ("Traits: " ++ String.fromInt pts) ]

        _ ->
            paragraph []
                [ text ("Traits: " ++ String.fromInt pts ++ ": " ++ traitNames) ]
