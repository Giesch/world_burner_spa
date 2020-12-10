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
import Html
import Html.Attributes
import Http
import Json.Decode as Decode
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Lifepath.GenSkills as GenSkills exposing (GenSkills)
import Model.Lifepath.LifepathIndex as LifepathIndex exposing (LifepathIndex)
import Model.Lifepath.Requirement as Requirement exposing (Requirement)
import Model.Lifepath.Resources as Resources exposing (Resources)
import Model.Lifepath.Skill as Skill exposing (Skill)
import Model.Lifepath.StatMod as StatMod exposing (StatMod)
import Model.Lifepath.Trait as Trait exposing (Trait)
import Model.Lifepath.Validation as Validation exposing (ValidPathList, ValidatedLifepath)
import Model.Lifepath.Years as Years exposing (Years)
import Model.Status as Status exposing (Status)
import Model.Worksheet as Worksheet exposing (Worksheet)
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
    , dnd : DnDList.Model
    , modalState : Maybe ModalState
    , worksheet : Maybe Worksheet
    }


type alias ModalState =
    { searchText : String
    , filteredPaths : List Lifepath
    , lifepathIndex : LifepathIndex
    , selectedLifepath : Int
    , option : ModalOption
    }


newModal : LifepathIndex -> ModalOption -> ModalState
newModal lifepathIndex option =
    { searchText = ""
    , filteredPaths =
        applyOptionFilter option <|
            LifepathIndex.lifepaths lifepathIndex
    , lifepathIndex = lifepathIndex
    , selectedLifepath = 0
    , option = option
    }


applyOptionFilter : ModalOption -> List Lifepath -> List Lifepath
applyOptionFilter option =
    case option of
        RequirementAt requirement _ ->
            List.filter
                (\lp -> Lifepath.mentionedIn lp requirement.predicate)

        _ ->
            identity


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
      , worksheet = Nothing
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
    | OpenModal ModalOption
    | SatisfyRequirement Requirement Int
    | SubmitModal (Maybe ( Lifepath, ModalState ))
    | EnteredModalSearchText String
    | SearchTimePassed String
    | SelectedModalLifepath Int
    | ArrowPress Direction
    | NoOp


type ModalOption
    = Before
    | After
    | RequirementAt Requirement Int


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
                    case Maybe.map Worksheet.lifepaths model.worksheet of
                        Nothing ->
                            ( model.dnd, [] )

                        Just paths ->
                            system.update dndMsg model.dnd paths

                worksheet =
                    Maybe.andThen (Worksheet.replaceLifepaths lifepaths)
                        model.worksheet
            in
            ( { model | dnd = dnd, worksheet = worksheet }
            , system.commands dnd
            )

        RemoveLifepath index ->
            let
                dropLifepath paths =
                    List.take index paths ++ List.drop (index + 1) paths

                newWorksheet =
                    Maybe.andThen (Worksheet.updateLifepaths dropLifepath)
                        model.worksheet
            in
            ( { model | worksheet = newWorksheet }, Cmd.none )

        OpenModal modalOption ->
            case model.searchableLifepaths of
                Status.Loaded searchableLifepaths ->
                    ( { model | modalState = Just <| newModal searchableLifepaths modalOption }
                    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus modalSearchId
                    )

                _ ->
                    ( model, Cmd.none )

        SatisfyRequirement requirement index ->
            case model.searchableLifepaths of
                Status.Loaded searchableLifepaths ->
                    let
                        option : ModalOption
                        option =
                            RequirementAt requirement index
                    in
                    ( { model | modalState = Just <| newModal searchableLifepaths option }
                    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus modalSearchId
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitModal Nothing ->
            ( { model | modalState = Nothing }, Cmd.none )

        SubmitModal (Just ( addedLifepath, modalState )) ->
            let
                unpackedLifepaths : List Lifepath
                unpackedLifepaths =
                    model.worksheet
                        |> Maybe.map Worksheet.lifepaths
                        |> Maybe.withDefault []
                        |> List.map .lifepath

                characterLifepaths : NonEmpty Lifepath
                characterLifepaths =
                    case modalState.option of
                        After ->
                            Common.appendIntoNonEmpty unpackedLifepaths addedLifepath

                        Before ->
                            ( addedLifepath, unpackedLifepaths )

                        RequirementAt _ index ->
                            Common.insertIntoNonEmpty unpackedLifepaths addedLifepath index
            in
            ( { model
                | modalState = Nothing
                , worksheet = Just <| Worksheet.new characterLifepaths
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
        , filteredPaths = applyOptionFilter modalState.option hits
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
            Maybe.map Worksheet.lifepaths model.worksheet
                |> Maybe.withDefault []
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
        , el [ paddingEach { edges | right = 20, bottom = 20 }, alignRight ] <|
            faintButton "Add Lifepath" (Just <| OpenModal After)
        , ghostView model.dnd unpackedLifepaths
        ]
    }


viewCharacterLifepaths : Model -> Element Msg
viewCharacterLifepaths model =
    el [ width fill, padding 20 ] <|
        case Maybe.map Worksheet.lifepaths model.worksheet of
            Nothing ->
                none

            Just lifepaths ->
                column [ width fill ] <|
                    [ el [ paddingEach { edges | bottom = 20 }, alignRight ] <|
                        faintButton "Add Lifepath" (Just <| OpenModal Before)
                    , el
                        [ width fill
                        , Border.width 1
                        , Border.color Colors.faint
                        , Border.rounded 8
                        ]
                      <|
                        column [ width fill ] <|
                            List.indexedMap (viewDraggableLifepath model.dnd) <|
                                lifepaths
                    ]


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
        selectedLifepath : Maybe Lifepath
        selectedLifepath =
            List.drop modalState.selectedLifepath modalState.filteredPaths
                |> List.head

        submission : Maybe ( Lifepath, ModalState )
        submission =
            Maybe.map (Tuple.pair modalState >> Common.swap) selectedLifepath
    in
    column
        [ width (fill |> minimum 600)
        , height (fill |> maximum 700)
        , Background.color Colors.white
        , Border.rounded 8
        ]
        [ heading "Add Lifepath"
        , column [ padding 40 ]
            [ Input.text
                [ htmlAttribute <| Html.Attributes.id modalSearchId
                , Common.onEnter <| SubmitModal submission
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
                    let
                        baseAttrs : List (Attribute Msg)
                        baseAttrs =
                            [ width fill, Events.onDoubleClick <| SubmitModal submission ]

                        additionalAttrs : List (Attribute Msg)
                        additionalAttrs =
                            if i == modalState.selectedLifepath then
                                [ Background.color Colors.faint ]

                            else
                                [ Events.onClick <| SelectedModalLifepath i ]
                    in
                    el (baseAttrs ++ additionalAttrs) <|
                        viewLifepath
                            { lifepath = lp
                            , id = "search-result-lp-" ++ String.fromInt i
                            , warnings = Validation.emptyWarnings
                            , dndOptions = Nothing
                            }
                )
                modalState.filteredPaths
        , row
            [ width fill
            , height fill
            , Border.rounded 8
            , Background.color Colors.white
            , spacing 20
            , padding 20
            ]
            [ faintButton "Cancel" <| Just (SubmitModal Nothing)
            , faintButton "Add" <| Just (SubmitModal submission)
            ]
        ]


ghostView : DnDList.Model -> List Validation.ValidatedLifepath -> Element Msg
ghostView dnd lifepaths =
    let
        draggedPath : Maybe Validation.ValidatedLifepath
        draggedPath =
            system.info dnd
                |> Maybe.andThen
                    (\{ dragIndex } ->
                        lifepaths
                            |> List.drop dragIndex
                            |> List.head
                    )
    in
    case draggedPath of
        Just { lifepath } ->
            row
                ([ Background.color Colors.white
                 , Border.color Colors.faint
                 , Border.rounded 8
                 , Border.width 1
                 , width shrink
                 , height (fill |> maximum 50)
                 , padding 20
                 ]
                    ++ (List.map htmlAttribute <| system.ghostStyles dnd)
                    -- NOTE order matters for this; we need our style to override ghostStyles
                    ++ [ htmlAttribute <| Html.Attributes.style "width" "auto" ]
                )
            <|
                [ Components.dragHandle []
                , text <| toTitleCase lifepath.name
                , el [ Font.size 18, paddingEach { edges | left = 20 } ] <|
                    (text <| "(" ++ toTitleCase lifepath.settingName ++ ")")
                ]

        Nothing ->
            none


heading : String -> Element Msg
heading h =
    el [ width fill, padding 20, Background.color Colors.faint ] <| text h


type alias DnDStyles =
    { dragStyles : List (Attribute Msg)
    , dropStyles : List (Attribute Msg)
    }


dndStyles : DnDList.Model -> Int -> DnDStyles
dndStyles dnd index =
    let
        id =
            dndId index
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                { dragStyles = []
                , dropStyles = List.map htmlAttribute <| system.dropEvents index id
                }

            else
                { dragStyles = [], dropStyles = [] }

        Nothing ->
            { dragStyles = List.map htmlAttribute <| system.dragEvents index id
            , dropStyles = []
            }


dndId : Int -> String
dndId index =
    "lp-drag-" ++ String.fromInt index


viewDraggableLifepath : DnDList.Model -> Int -> ValidatedLifepath -> Element Msg
viewDraggableLifepath dnd dragIndex { lifepath, warnings } =
    let
        { dragStyles, dropStyles } =
            dndStyles dnd dragIndex

        dndOptions =
            { dragIndex = dragIndex, dragStyles = dragStyles, dropStyles = dropStyles }
    in
    viewLifepath
        { dndOptions = Just dndOptions
        , lifepath = lifepath
        , id = dndId dragIndex
        , warnings = warnings
        }


type alias LifepathOptions =
    { dndOptions : Maybe (Lifepath.DnDOptions Msg)
    , lifepath : Lifepath
    , id : String
    , warnings : Lifepath.Warnings
    }


viewLifepath : LifepathOptions -> Element Msg
viewLifepath opts =
    Lifepath.view
        { dndOptions = opts.dndOptions
        , lifepath = opts.lifepath
        , id = opts.id
        , warnings = opts.warnings
        , onClickRequirement = SatisfyRequirement
        , onDelete = RemoveLifepath
        }
