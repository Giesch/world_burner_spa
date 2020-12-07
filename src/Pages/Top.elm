module Pages.Top exposing (Model, Msg, Params, page)

import Api
import Browser.Dom
import Browser.Events
import Colors
import Common exposing (edges)
import Components
import Dict exposing (Dict)
import DnDList
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon
import FontAwesome.Regular
import FontAwesome.Solid
import Html
import Html.Attributes
import Http
import Json.Decode as Decode
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Lifepath.GenSkills as GenSkills exposing (GenSkills)
import Model.Lifepath.LifepathIndex as LifepathIndex exposing (LifepathIndex)
import Model.Lifepath.Resources as Resources exposing (Resources)
import Model.Lifepath.Skill as Skill exposing (Skill)
import Model.Lifepath.StatMod as StatMod exposing (StatMod)
import Model.Lifepath.Trait as Trait exposing (Trait)
import Model.Lifepath.Validation as Validation exposing (ValidPathList, ValidatedLifepath)
import Model.Lifepath.Years as Years exposing (Years)
import Model.Status as Status exposing (Status)
import Process
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import String.Extra exposing (toTitleCase)
import Task


page : Page Params Model Msg
page =
    Page.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- INIT


type alias Params =
    ()


type alias Model =
    { searchableLifepaths : Status LifepathIndex
    , name : String
    , characterLifepaths : ValidPathList
    , dnd : DnDList.Model
    , modalState : Maybe ModalState
    }


type alias ModalState =
    { searchText : String
    , filteredPaths : List Lifepath
    , lifepathIndex : LifepathIndex
    , selectedLifepath : Int
    }


defaultModal : LifepathIndex -> ModalState
defaultModal lifepathIndex =
    { searchText = ""
    , filteredPaths = LifepathIndex.lifepaths lifepathIndex
    , lifepathIndex = lifepathIndex
    , selectedLifepath = 0
    }


system : DnDList.System ValidatedLifepath Msg
system =
    DnDList.create dndConfig DnDMsg


dndConfig : DnDList.Config ValidatedLifepath
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { searchableLifepaths = Status.Loading
      , name = ""
      , characterLifepaths = Validation.validate []
      , dnd = system.model
      , modalState = Nothing
      }
    , Api.lifepaths GotLifepaths
    )



-- UPDATE


type Msg
    = GotLifepaths (Result Http.Error (List Lifepath))
    | DnDMsg DnDList.Msg
    | EnteredName String
    | RemoveLifepath Int
    | OpenModal
    | SubmitModal (Maybe Lifepath)
    | EnteredModalSearchText String
    | SearchTimePassed String
    | SelectedModalLifepath Int
    | ArrowPress Direction
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotLifepaths (Ok allLifepaths) ->
            let
                searchableLifepaths : Status LifepathIndex
                searchableLifepaths =
                    case LifepathIndex.new allLifepaths of
                        Just searchIndex ->
                            Status.Loaded searchIndex

                        Nothing ->
                            Status.Failed
            in
            ( { model | searchableLifepaths = searchableLifepaths }, Cmd.none )

        GotLifepaths (Err _) ->
            ( { model | searchableLifepaths = Status.Failed }, Cmd.none )

        EnteredName name ->
            ( { model | name = name }, Cmd.none )

        DnDMsg dndMsg ->
            let
                ( dnd, lifepaths ) =
                    system.update dndMsg model.dnd <| Validation.unpack model.characterLifepaths
            in
            ( { model | dnd = dnd, characterLifepaths = Validation.revalidate lifepaths }
            , system.commands dnd
            )

        RemoveLifepath index ->
            let
                unpacked =
                    Validation.unpack model.characterLifepaths

                lifepaths =
                    List.take index unpacked
                        ++ List.drop (index + 1) unpacked
            in
            ( { model | characterLifepaths = Validation.revalidate lifepaths }, Cmd.none )

        OpenModal ->
            case model.searchableLifepaths of
                Status.Loaded searchableLifepaths ->
                    ( { model | modalState = Just <| defaultModal searchableLifepaths }
                    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus modalSearchId
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitModal Nothing ->
            ( { model | modalState = Nothing }, Cmd.none )

        SubmitModal (Just addedLifepath) ->
            ( { model
                | modalState = Nothing
                , characterLifepaths =
                    Validation.validate <| List.map .lifepath (Validation.unpack model.characterLifepaths) ++ [ addedLifepath ]
              }
            , Cmd.none
            )

        SelectedModalLifepath index ->
            updateModal
                (\modalState ->
                    ( { modalState | selectedLifepath = index }
                    , Cmd.none
                    )
                )
                model

        SearchTimePassed searchText ->
            updateModal
                (\modalState ->
                    ( searchTimePassed searchText modalState
                    , Cmd.none
                    )
                )
                model

        EnteredModalSearchText searchText ->
            updateModal
                (\modalState ->
                    ( { modalState | searchText = searchText }
                    , beginSearchDebounce searchText
                    )
                )
                model

        ArrowPress direction ->
            updateModal (handleArrow direction) model


handleArrow : Direction -> ModalState -> ( ModalState, Cmd Msg )
handleArrow direction modalState =
    let
        selectedLifepath : Int
        selectedLifepath =
            case direction of
                Up ->
                    modalState.selectedLifepath - 1

                Down ->
                    modalState.selectedLifepath + 1

                Other ->
                    modalState.selectedLifepath

        clamp : Int -> Int
        clamp =
            Common.clamp ( 0, List.length modalState.filteredPaths - 1 )
    in
    ( { modalState | selectedLifepath = clamp selectedLifepath }, Cmd.none )


beginSearchDebounce : String -> Cmd Msg
beginSearchDebounce searchText =
    Process.sleep 100
        |> Task.perform (\_ -> SearchTimePassed searchText)


searchTimePassed : String -> ModalState -> ModalState
searchTimePassed oldSearchText modalState =
    if oldSearchText == modalState.searchText then
        searchLifepaths modalState

    else
        modalState


searchLifepaths : ModalState -> ModalState
searchLifepaths modalState =
    let
        ( newIndex, hits ) =
            LifepathIndex.search
                modalState.searchText
                modalState.lifepathIndex
    in
    { modalState
        | selectedLifepath = 0
        , filteredPaths = hits
        , lifepathIndex = newIndex
    }


updateModal : (ModalState -> ( ModalState, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
updateModal fn model =
    case model.modalState of
        Nothing ->
            ( model, Cmd.none )

        Just state ->
            let
                ( newState, cmd ) =
                    fn state
            in
            ( { model | modalState = Just newState }, cmd )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    case model.modalState of
        Just _ ->
            { shared | modalOpen = True }

        Nothing ->
            { shared | modalOpen = False }


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    if shared.modalOpen then
        ( model, Cmd.none )

    else
        ( { model | modalState = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ system.subscriptions model.dnd
        , Browser.Events.onKeyDown keyDecoder
            |> Sub.map ArrowPress
        ]


type Direction
    = Up
    | Down
    | Other


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other



-- VIEW


view : Model -> Document Msg
view model =
    let
        unpackedLifepaths =
            Validation.unpack model.characterLifepaths
    in
    { title = "Charred Knockoff"
    , modal = Maybe.map viewModal model.modalState
    , body =
        [ heading "Character"
        , row [ padding 20, spacing 20 ]
            [ Input.text []
                { onChange = EnteredName
                , text = model.name
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Name:"
                }
            , text <| "Age: " ++ String.fromInt (Years.age <| List.map (.lifepath >> .years) unpackedLifepaths)
            ]
        , heading "Lifepaths"
        , viewCharacterLifepaths model
        , el [ paddingEach { edges | right = 20, bottom = 20 }, alignRight ] <| faintButton "Add Lifepath" (Just OpenModal)
        , ghostView model.dnd unpackedLifepaths
        ]
    }


viewCharacterLifepaths : Model -> Element Msg
viewCharacterLifepaths model =
    el [ width fill, padding 20 ] <|
        case Validation.unpack model.characterLifepaths of
            [] ->
                none

            lifepaths ->
                el
                    [ width fill
                    , Border.width 1
                    , Border.color Colors.faint
                    , Border.rounded 8
                    ]
                <|
                    column [ width fill ] <|
                        List.indexedMap
                            (\i { lifepath, warnings } ->
                                viewDraggableLifepath
                                    { dnd = model.dnd
                                    , maybeIndex = Just i
                                    , lifepath = lifepath
                                    , warnings = warnings
                                    }
                            )
                            lifepaths


faintButton : String -> Maybe Msg -> Element Msg
faintButton label onPress =
    Input.button
        [ alignRight
        , Background.color Colors.faint
        , Border.rounded 8
        , paddingXY 15 10
        ]
        { onPress = onPress
        , label = text label
        }


modalSearchId : String
modalSearchId =
    "lifepath-search"


viewModal : ModalState -> Element Msg
viewModal modalState =
    let
        selectedLifepath =
            List.drop modalState.selectedLifepath modalState.filteredPaths
                |> List.head
    in
    column
        [ width (fill |> minimum 600)
        , height (fill |> maximum 700)
        , Background.color Colors.white
        , Border.rounded 8
        ]
    <|
        [ heading "Add Lifepath"
        , column [ padding 40 ]
            [ Input.text
                [ htmlAttribute <| Html.Attributes.id modalSearchId
                , Common.onEnter <| SubmitModal selectedLifepath
                ]
                { onChange = EnteredModalSearchText
                , text = modalState.searchText
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Search"
                }
            ]
        , column
            [ height fill
            , width fill
            , Background.color Colors.white
            , clip
            , scrollbarY
            ]
          <|
            List.indexedMap
                (\i lp ->
                    if i == modalState.selectedLifepath then
                        el [ width fill, Background.color Colors.faint ] <|
                            viewSearchResultLifepath lp ("search-result-lp-" ++ String.fromInt i)

                    else
                        el [ width fill, Events.onMouseDown <| SelectedModalLifepath i ] <|
                            viewSearchResultLifepath lp ("search-result-lp-" ++ String.fromInt i)
                )
                modalState.filteredPaths
        , row [ width fill, height fill, Background.color Colors.white, spacing 20, padding 20 ] <|
            [ faintButton "Cancel" <| Just (SubmitModal Nothing)
            , faintButton "Add" <| Just (SubmitModal selectedLifepath)
            ]
        ]


ghostView : DnDList.Model -> List Validation.ValidatedLifepath -> Element Msg
ghostView dnd lifepaths =
    let
        maybePath : Maybe ( Int, Validation.ValidatedLifepath )
        maybePath =
            system.info dnd
                |> Maybe.andThen
                    (\{ dragIndex } ->
                        lifepaths |> List.drop dragIndex |> List.head |> Maybe.map (Tuple.pair dragIndex)
                    )
    in
    case maybePath of
        Just ( index, { lifepath, warnings } ) ->
            el
                (Background.color Colors.white
                    :: Border.color Colors.faint
                    :: Border.rounded 8
                    :: Border.width 1
                    :: (List.map htmlAttribute <| system.ghostStyles dnd)
                )
            <|
                viewDraggableLifepath
                    { dnd = dnd
                    , maybeIndex = Just index
                    , lifepath = lifepath
                    , warnings = warnings
                    }

        Nothing ->
            none


heading : String -> Element Msg
heading h =
    el
        [ width fill
        , centerX
        , padding 20
        , Background.color Colors.faint
        ]
    <|
        text h


{-| For non-ghost dnd list elements. Goes on the handle
-}
dndStyles : DnDList.Model -> Int -> List (Attribute Msg)
dndStyles dnd index =
    let
        id =
            dndId <| Just index
    in
    List.map htmlAttribute <|
        case system.info dnd of
            Just { dragIndex } ->
                if dragIndex /= index then
                    system.dropEvents index id

                else
                    []

            Nothing ->
                system.dragEvents index id


dndId : Maybe Int -> String
dndId maybeIndex =
    case maybeIndex of
        Just index ->
            "lp-drag-" ++ String.fromInt index

        Nothing ->
            "lp-drag-ghost"


type alias LifepathOptions =
    { dnd : DnDList.Model
    , maybeIndex : Maybe Int
    , lifepath : Lifepath
    , warnings : List String
    }


viewSearchResultLifepath : Lifepath -> String -> Element Msg
viewSearchResultLifepath lifepath id =
    viewInnerLifepath
        { lifepath = lifepath
        , dndStyles = []
        , id = id
        , removeBtnIndex = Nothing
        , warnings = []
        , unselectable = False
        }


type alias InnerLifepathOptions =
    { dndStyles : List (Attribute Msg)
    , lifepath : Lifepath
    , id : String
    , removeBtnIndex : Maybe (Maybe Int)
    , warnings : List String
    , unselectable : Bool
    }


viewInnerLifepath : InnerLifepathOptions -> Element Msg
viewInnerLifepath opts =
    let
        baseRowAttrs =
            [ width fill
            , spacing 10
            , paddingXY 20 20
            , htmlAttribute <| Html.Attributes.id opts.id
            ]
                ++ opts.dndStyles

        rowAttrs =
            if opts.unselectable then
                baseRowAttrs ++ Common.userSelectNone

            else
                baseRowAttrs
    in
    row rowAttrs <|
        [ column [ width fill, spacing 5 ] <|
            [ row [ width fill ]
                [ text <| toTitleCase opts.lifepath.name
                , el [ Font.size 18, paddingEach { edges | left = 20 } ] <|
                    (text <| "(" ++ toTitleCase opts.lifepath.settingName ++ ")")
                ]
            , textColumn [ width fill, Font.size 18 ]
                [ row [ width fill, spacing 10, Font.size 18 ]
                    [ text <| Years.toString opts.lifepath.years
                    , text <| Resources.toString opts.lifepath.res
                    , text <| StatMod.toString opts.lifepath.statMod
                    ]
                , paragraph [] <|
                    [ text <| "Skills: "
                    , GenSkills.toString opts.lifepath.genSkills
                        |> Maybe.map (\genText -> text (genText ++ ", "))
                        |> Maybe.withDefault none
                    , text <| String.fromInt opts.lifepath.skillPts ++ " pts: "
                    ]
                        ++ (List.intersperse (text ", ") <| List.map viewSkill opts.lifepath.skills)
                , paragraph [] <|
                    [ text <| "Traits: "
                    , text <|
                        String.fromInt opts.lifepath.traitPts
                            ++ " pts: "
                            ++ (String.join ", " <| List.map (Trait.name >> toTitleCase) opts.lifepath.traits)
                    ]
                , paragraph [ width fill ] <|
                    [ text "Leads: "
                    , text <| String.join ", " <| List.map (.settingName >> toTitleCase) opts.lifepath.leads
                    ]
                , case opts.lifepath.requirement of
                    Nothing ->
                        none

                    Just requirement ->
                        paragraph [] <|
                            [ text <| "Requires: "
                            , text <| requirement.description
                            ]
                ]
            ]
        , warningIcon opts.warnings
        , case opts.removeBtnIndex of
            Nothing ->
                none

            Just maybeIndex ->
                Input.button
                    [ alignRight, alignTop ]
                    { onPress = Maybe.map RemoveLifepath maybeIndex
                    , label = lifepathIcon FontAwesome.Regular.trashAlt
                    }
        ]


viewSkill : Skill -> Element msg
viewSkill skill =
    let
        superScript script =
            Element.html <| Html.sup [] <| [ Html.text <| String.fromChar script ]

        suffix : Maybe Char
        suffix =
            case ( skill.magical, skill.training ) of
                ( False, False ) ->
                    Nothing

                ( True, False ) ->
                    Just Common.sectionSign

                ( False, True ) ->
                    Just Common.dagger

                ( True, True ) ->
                    -- NOTE this case no longer shows up in the book
                    -- (double dagger will be taken for spell songs)
                    Just Common.tripleDagger
    in
    Element.row [] <|
        List.filterMap identity
            [ Just <| Element.text <| Skill.toString skill
            , Maybe.map superScript suffix
            ]


warningIcon : List String -> Element msg
warningIcon warnings =
    el
        [ alignRight
        , alignTop
        , Components.tooltip onLeft (warningsTooltip warnings)
        , transparent (List.isEmpty warnings)
        ]
    <|
        lifepathIcon FontAwesome.Solid.exclamationTriangle


lifepathIcon : Icon.Icon -> Element msg
lifepathIcon icon =
    el [ alignRight, alignTop, paddingEach { edges | right = 10 } ] <|
        (html <| Icon.viewStyled [ Colors.darkenedHtml ] icon)


warningsTooltip : List String -> Element msg
warningsTooltip warnings =
    el
        [ Background.color Colors.white
        , Font.color Colors.black
        , padding 5
        , Border.rounded 5
        , Font.size 16
        , Border.shadow
            { offset = ( 0, 3 )
            , blur = 6
            , size = 0
            , color = rgba 0 0 0 0.32
            }
        ]
    <|
        column [ padding 5, spacing 5 ] (List.map text warnings)


viewDraggableLifepath : LifepathOptions -> Element Msg
viewDraggableLifepath { dnd, maybeIndex, lifepath, warnings } =
    viewInnerLifepath
        { dndStyles =
            Maybe.map (dndStyles dnd) maybeIndex
                |> Maybe.withDefault []
        , lifepath = lifepath
        , id = dndId maybeIndex
        , removeBtnIndex = Just maybeIndex
        , warnings = warnings
        , unselectable = True
        }
