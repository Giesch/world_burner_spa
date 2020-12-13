module ValidationTest exposing (..)

import Expect
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath exposing (Lifepath)
import Model.Lifepath.GenSkills as GenSkills exposing (GenSkills)
import Model.Lifepath.Lead as Lead exposing (Lead)
import Model.Lifepath.Resources as Resources exposing (Resources)
import Model.Lifepath.StatMod as StatMod exposing (StatMod)
import Model.Lifepath.Validation as Validation
import Model.Lifepath.Years as Years exposing (Years)
import Test exposing (..)


checksLeads : Test
checksLeads =
    describe "Leads"
        [ test "a missing lead produces a warning on the preceding lifepath" <|
            \_ ->
                ( bornNowhere, [ somebody ] )
                    |> haveLeads
                    |> Expect.equal [ False, True ]
        , test "staying in setting does not require a lead" <|
            \_ ->
                ( bornNowhere, [ nowhereMan ] )
                    |> haveLeads
                    |> Expect.equal [ True, True ]
        , test "a valid lead doesn't produce a warning" <|
            \_ ->
                ( bornNowhere, [ nowhereMan, somebody ] )
                    |> haveLeads
                    |> Expect.equal [ True, True, True ]
        ]


{-| Returns whether each lifepath has the necessary lead for the following lifepath.
The last lifepath should default to True.
-}
haveLeads : NonEmpty Lifepath -> List Bool
haveLeads paths =
    paths
        |> Validation.addWarnings
        |> Validation.unpack
        |> NonEmpty.toList
        |> List.map (.warnings >> .hasLead)


bornNowhere : Lifepath
bornNowhere =
    leadTestLifepath
        { name = "Born Nowhere"
        , id = 0
        , settingId = 0
        , settingName = "nowhere"
        , leads = []
        }


nowhereMan : Lifepath
nowhereMan =
    leadTestLifepath
        { name = "Nowhere Man"
        , id = 1
        , settingId = 0
        , settingName = "nowhere"
        , leads = [ { settingId = 1, settingName = "somewhere" } ]
        }


somebody : Lifepath
somebody =
    leadTestLifepath
        { name = "Somebody"
        , id = 2
        , settingId = 1
        , settingName = "somewhere"
        , leads = []
        }


type alias LeadTestLifepath =
    { name : String
    , id : Int
    , settingId : Int
    , settingName : String
    , leads : List Lead
    }


leadTestLifepath : LeadTestLifepath -> Lifepath
leadTestLifepath props =
    { name = props.name
    , id = props.id
    , settingId = props.settingId
    , settingName = props.settingName
    , leads = props.leads
    , statMod = StatMod.none
    , years = Years.count 10
    , res = Resources.points 0
    , born = False
    , genSkills = GenSkills.points 0
    , skillPts = 0
    , traitPts = 0
    , page = 0
    , skills = []
    , traits = []
    , requirement = Nothing
    }
