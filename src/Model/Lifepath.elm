module Model.Lifepath exposing
    ( Lead
    , Lifepath
    , LifepathSkill
    , decoder
    , lifepathWidth
    , view
    )

import Colors exposing (..)
import Common
import DnD.Beacon as Beacon
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Model.Lifepath.GenSkills as GenSkills exposing (GenSkills)
import Model.Lifepath.Requirement as Requirement exposing (Requirement)
import Model.Lifepath.Resources as Resources exposing (Resources)
import Model.Lifepath.StatMod as StatMod exposing (StatMod)
import Model.Lifepath.Years as Years exposing (Years)
import Model.Trait as Trait exposing (Trait)
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
    , skills : List LifepathSkill
    , traits : List LifepathTrait
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
    , skills : List LifepathSkill
    , traits : List LifepathTrait
    , born : Bool
    , requirement : Maybe Requirement
    }


type alias LifepathSkill =
    { skillId : Int
    , displayName : String
    , magical : Bool
    , training : Bool
    }


type LifepathTrait
    = Entry Int
    | Entryless String


type alias Lead =
    { settingName : String
    , settingId : Int
    , settingPage : Int
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
        -- TODO
        -- |> required "leads" (Decode.list leadDecoder)
        |> required "leads" (Decode.succeed [])
        |> required "genSkills" GenSkills.decoder
        |> required "skillPts" Decode.int
        |> required "traitPts" Decode.int
        |> required "skillList" (Decode.list lifepathSkillDecoder)
        |> required "traitList" (Decode.list lifepathTraitDecoder)
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
            -- List.map .displayName json.skills
            []

        traits : List String
        traits =
            -- List.map Trait.name json.traits
            []
    in
    json.name :: json.settingName :: skills ++ traits


leadDecoder : Decoder Lead
leadDecoder =
    Decode.succeed Lead
        |> required "settingName" Decode.string
        |> required "settingId" Decode.int
        |> required "settingPage" Decode.int


lifepathSkillDecoder : Decoder LifepathSkill
lifepathSkillDecoder =
    Decode.succeed LifepathSkill
        |> required "skillId" Decode.int
        |> required "displayName" Decode.string
        |> required "magical" Decode.bool
        |> required "training" Decode.bool


lifepathTraitDecoder : Decoder LifepathTrait
lifepathTraitDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen traitKindsDecoder


traitKindsDecoder : String -> Decoder LifepathTrait
traitKindsDecoder kind =
    case kind of
        "entry" ->
            Decode.map Entry (Decode.field "value" Decode.int)

        "entryless" ->
            Decode.map Entryless (Decode.field "value" Decode.string)

        k ->
            Decode.fail <| "Invalid lifepath trait kind: " ++ k



-- VIEW


lifepathWidth : Element.Length
lifepathWidth =
    Element.px 300


view : Maybe Beacon.DragBeaconLocation -> Lifepath -> Element msg
view beaconLocation lifepath =
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
            case beaconLocation of
                Just location ->
                    Beacon.dragBeacon location :: defaultAttrs

                Nothing ->
                    defaultAttrs
    in
    column attrs
        [ text <| toTitleCase lifepath.name ++ " (" ++ toTitleCase lifepath.settingName ++ ")"
        , row [ width fill, spaceEvenly ]
            [ text <| Years.toString lifepath.years
            , text <| Resources.toString lifepath.res
            , viewLifepathStat lifepath.statMod
            ]
        , viewSkills lifepath.skillPts lifepath.skills
        , viewTraits lifepath.traitPts lifepath.traits
        , viewLeads lifepath.leads
        ]


viewSkills : Int -> List LifepathSkill -> Element msg
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


viewLifepathStat : StatMod -> Element msg
viewLifepathStat statMod =
    case statMod of
        StatMod.NoMod ->
            text <| "stat: --"

        mod ->
            -- TODO this is so gross
            text <| "stat: " ++ viewStatMod mod


viewStatMod : StatMod -> String
viewStatMod statMod =
    let
        ( kind, value ) =
            case statMod of
                StatMod.Physical v ->
                    ( "P", v )

                StatMod.Mental v ->
                    ( "M", v )

                StatMod.Either v ->
                    ( "M/P", v )

                StatMod.Both v ->
                    ( "M,P", v )

                StatMod.NoMod ->
                    ( "", 0 )

        prefix =
            -- zero is not a permitted value in the db
            if value > 0 then
                "+"

            else
                "-"
    in
    if String.length kind > 0 then
        prefix ++ String.fromInt value ++ kind

    else
        ""


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


viewTraits : Int -> List LifepathTrait -> Element msg
viewTraits pts traits =
    let
        traitNames =
            -- TODO
            --     String.join ", " <| List.map (\tr -> toTitleCase <| Trait.name tr) traits
            String.join ", " <| List.map (\tr -> "trait") traits
    in
    case ( pts, List.length traits ) of
        ( 0, 0 ) ->
            none

        ( _, 0 ) ->
            paragraph []
                [ text ("Traits: " ++ String.fromInt pts) ]

        _ ->
            paragraph []
                [ text ("Traits: " ++ String.fromInt pts ++ ": " ++ traitNames) ]
