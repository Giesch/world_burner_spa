module Pages.Top exposing (Model, Msg, Params, page)

import Api
import Browser.Dom
import Browser.Events
import Colors
import Common exposing (corners, edges)
import Components
import Dict exposing (Dict)
import DnDList
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
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
import Model.Lifepath.Validation as Validation exposing (PathWithWarnings, ValidatedLifepaths)
import Model.Lifepath.Years as Years exposing (Years)
import Model.Status as Status exposing (Status)
import Model.Worksheet as Worksheet exposing (Worksheet)
import Model.Worksheet.Health as Health
import Model.Worksheet.Shade as Shade exposing (Shade)
import Model.Worksheet.ShadedStats as ShadedStats exposing (ShadedStats)
import Model.Worksheet.Stat as Stat exposing (Stat)
import Model.Worksheet.Steel as Steel
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
    , modalState : Maybe Modal
    , worksheet : Maybe Worksheet
    }


type Modal
    = LifepathModal LifepathModalState
    | HealthQuestionModal ( ShadedStats, Health.Answers )
    | SteelQuestionModal ( ShadedStats, Steel.Answers )


type alias LifepathModalState =
    { searchText : String
    , filteredPaths : List Lifepath
    , lifepathIndex : LifepathIndex
    , selectedLifepath : Int
    , option : LifepathModalOption
    }


newLifepathModal : LifepathIndex -> LifepathModalOption -> LifepathModalState
newLifepathModal lifepathIndex option =
    { searchText = ""
    , filteredPaths = applyOptionFilter option <| LifepathIndex.lifepaths lifepathIndex
    , lifepathIndex = lifepathIndex
    , selectedLifepath = 0
    , option = option
    }


applyOptionFilter : LifepathModalOption -> List Lifepath -> List Lifepath
applyOptionFilter option =
    case option of
        RequirementAt requirement _ ->
            List.filter
                (\lp -> Lifepath.mentionedIn lp requirement.predicate)

        _ ->
            identity


system : DnDList.System PathWithWarnings Msg
system =
    DnDList.create dndConfig DnDMsg


dndConfig : DnDList.Config PathWithWarnings
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
    | OpenLifepathModal LifepathModalOption
    | SatisfyRequirement Requirement Int
    | SubmitLifepathModal (Maybe ( LifepathModalOption, Lifepath ))
    | EnteredModalSearchText String
    | SearchTimePassed String
    | SelectedModalLifepath Int
    | ArrowPress Direction
    | ChangeStat Stat Int
    | DistributeStats
    | ToggleShade Stat
    | OpenHealthModal
    | UpdateHealthAnswers Health.Answers
    | SubmitHealthModal (Maybe Health.Answers)
    | OpenSteelModal
    | UpdateSteelAnswers Steel.Answers
    | SubmitSteelModal (Maybe Steel.Answers)
    | ToggleSteelShade Shade
    | NoOp


type LifepathModalOption
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
                    Maybe.map2 Worksheet.replaceLifepaths
                        (NonEmpty.fromList <| List.map .lifepath lifepaths)
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

        OpenLifepathModal modalOption ->
            case model.searchableLifepaths of
                Status.Loaded searchableLifepaths ->
                    let
                        modalState : Maybe Modal
                        modalState =
                            newLifepathModal searchableLifepaths modalOption
                                |> LifepathModal
                                |> Just
                    in
                    ( { model | modalState = modalState }
                    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus modalSearchId
                    )

                _ ->
                    ( model, Cmd.none )

        SatisfyRequirement requirement index ->
            case model.searchableLifepaths of
                Status.Loaded searchableLifepaths ->
                    let
                        option : LifepathModalOption
                        option =
                            RequirementAt requirement index

                        modalState : Maybe Modal
                        modalState =
                            newLifepathModal searchableLifepaths option
                                |> LifepathModal
                                |> Just
                    in
                    ( { model | modalState = modalState }
                    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus modalSearchId
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitLifepathModal Nothing ->
            ( { model | modalState = Nothing }, Cmd.none )

        SubmitLifepathModal (Just ( option, addedLifepath )) ->
            let
                unpackedLifepaths : List Lifepath
                unpackedLifepaths =
                    model.worksheet
                        |> Maybe.map Worksheet.lifepaths
                        |> Maybe.withDefault []
                        |> List.map .lifepath

                characterLifepaths : NonEmpty Lifepath
                characterLifepaths =
                    case option of
                        After ->
                            Common.appendIntoNonEmpty
                                unpackedLifepaths
                                addedLifepath

                        Before ->
                            ( addedLifepath, unpackedLifepaths )

                        RequirementAt _ index ->
                            Common.insertIntoNonEmpty
                                unpackedLifepaths
                                addedLifepath
                                index
            in
            ( { model
                | modalState = Nothing
                , worksheet =
                    model.worksheet
                        |> Maybe.map (Worksheet.replaceLifepaths characterLifepaths)
                        |> Maybe.withDefault (Worksheet.new characterLifepaths)
                        |> Just
              }
            , Cmd.none
            )

        SelectedModalLifepath index ->
            updateLifepathModal
                (\modalState ->
                    ( { modalState | selectedLifepath = index }
                    , Cmd.none
                    )
                )
                model

        SearchTimePassed searchText ->
            updateLifepathModal
                (\modalState ->
                    ( searchTimePassed searchText modalState
                    , Cmd.none
                    )
                )
                model

        EnteredModalSearchText searchText ->
            updateLifepathModal
                (\modalState ->
                    ( { modalState | searchText = searchText }
                    , beginSearchDebounce searchText
                    )
                )
                model

        ArrowPress direction ->
            updateLifepathModal (handleArrow direction) model

        ChangeStat stat val ->
            ( updateWorksheet (Worksheet.changeStat stat val) model
            , Cmd.none
            )

        DistributeStats ->
            ( updateWorksheet Worksheet.distributeStats model
            , Cmd.none
            )

        ToggleShade stat ->
            ( updateWorksheet (Worksheet.toggleShade stat) model
            , Cmd.none
            )

        OpenHealthModal ->
            case model.worksheet of
                Just worksheet ->
                    let
                        modalState : Maybe Modal
                        modalState =
                            Just <|
                                HealthQuestionModal
                                    ( Worksheet.shadedStats worksheet
                                    , Worksheet.healthAnswers worksheet
                                    )
                    in
                    ( { model | modalState = modalState }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateHealthAnswers newAnswers ->
            case model.modalState of
                Just (HealthQuestionModal ( stats, _ )) ->
                    ( { model | modalState = Just <| HealthQuestionModal ( stats, newAnswers ) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitHealthModal submission ->
            updateAnswers
                Worksheet.updateHealthAnswers
                submission
                model

        OpenSteelModal ->
            case model.worksheet of
                Just worksheet ->
                    let
                        modalState : Maybe Modal
                        modalState =
                            Just <|
                                SteelQuestionModal
                                    ( Worksheet.shadedStats worksheet
                                    , Worksheet.steelAnswers worksheet
                                    )
                    in
                    ( { model | modalState = modalState }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSteelAnswers newAnswers ->
            case model.modalState of
                Just (SteelQuestionModal ( stats, _ )) ->
                    ( { model | modalState = Just <| SteelQuestionModal ( stats, newAnswers ) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitSteelModal submission ->
            updateAnswers
                Worksheet.updateSteelAnswers
                submission
                model

        ToggleSteelShade shade ->
            let
                worksheet : Maybe Worksheet
                worksheet =
                    Maybe.map (Worksheet.toggleSteelShade shade) model.worksheet
            in
            ( { model | worksheet = worksheet }, Cmd.none )


updateWorksheet : (Worksheet -> Worksheet) -> Model -> Model
updateWorksheet fn model =
    { model | worksheet = Maybe.map fn model.worksheet }


updateAnswers :
    (answers -> Worksheet -> Worksheet)
    -> Maybe answers
    -> Model
    -> ( Model, Cmd msg )
updateAnswers fn submission model =
    let
        newWorksheet : Maybe Worksheet
        newWorksheet =
            case ( submission, model.worksheet ) of
                ( Just answers, Just worksheet ) ->
                    Just <| fn answers worksheet

                ( Nothing, Just worksheet ) ->
                    Just worksheet

                _ ->
                    Nothing
    in
    ( { model | worksheet = newWorksheet, modalState = Nothing }
    , Cmd.none
    )


handleArrow : Direction -> LifepathModalState -> ( LifepathModalState, Cmd Msg )
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
    Process.sleep 250
        |> Task.perform (\_ -> SearchTimePassed searchText)


searchTimePassed : String -> LifepathModalState -> LifepathModalState
searchTimePassed oldSearchText modalState =
    if oldSearchText == modalState.searchText then
        searchLifepaths modalState

    else
        modalState


searchLifepaths : LifepathModalState -> LifepathModalState
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


updateLifepathModal :
    (LifepathModalState -> ( LifepathModalState, Cmd Msg ))
    -> Model
    -> ( Model, Cmd Msg )
updateLifepathModal fn model =
    case model.modalState of
        Just (LifepathModal state) ->
            let
                ( newState, cmd ) =
                    fn state
            in
            ( { model | modalState = Just <| LifepathModal newState }, cmd )

        _ ->
            ( model, Cmd.none )


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
        List.concat
            [ viewCharacterAndLifepaths model
            , model.worksheet
                |> Maybe.map viewWorksheet
                |> Maybe.withDefault []
            , [ ghostView model.dnd unpackedLifepaths ] -- NOTE this has to be last
            ]
    }


viewCharacterAndLifepaths : Model -> List (Element Msg)
viewCharacterAndLifepaths model =
    [ topHeading "Character"
    , row [ padding 20, spacing 20 ]
        [ Input.text []
            { onChange = EnteredName
            , text = model.name
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Name:"
            }
        , text <| "Age: " ++ viewAge model.worksheet
        ]
    , heading "Lifepaths"
    , viewCharacterLifepaths model.worksheet model.dnd
    , el [ paddingEach { edges | right = 20, bottom = 20 }, alignRight ] <|
        Components.faintButton "Add Lifepath" <|
            OpenLifepathModal After
    ]


viewAge : Maybe Worksheet -> String
viewAge maybeSheet =
    maybeSheet
        |> Maybe.map Worksheet.age
        |> Maybe.withDefault 0
        |> String.fromInt


viewWorksheet : Worksheet -> List (Element Msg)
viewWorksheet worksheet =
    heading "Stats and Attributes"
        :: Worksheet.view
            { worksheet = worksheet
            , distributeStats = DistributeStats
            , toggleStatShade = ToggleShade
            , changeStat = ChangeStat
            , openHealthModal = OpenHealthModal
            , openSteelModal = OpenSteelModal
            , toggleSteelShade = ToggleSteelShade
            }


type alias StatRow =
    { stat : Stat
    , value : Int
    , shade : Shade
    }


viewCharacterLifepaths : Maybe Worksheet -> DnDList.Model -> Element Msg
viewCharacterLifepaths worksheet dnd =
    el [ width fill, padding 20 ] <|
        case Maybe.map Worksheet.lifepaths worksheet of
            Nothing ->
                none

            Just lifepaths ->
                column [ width fill ] <|
                    [ el [ paddingEach { edges | bottom = 20 }, alignRight ] <|
                        Components.faintButton "Add Lifepath" <|
                            OpenLifepathModal Before
                    , el
                        [ width fill
                        , Border.width 1
                        , Border.color Colors.faint
                        , Border.rounded 8
                        ]
                      <|
                        column [ width fill ] <|
                            List.indexedMap (viewDraggableLifepath dnd) lifepaths
                    ]


modalSearchId : String
modalSearchId =
    "lifepath-search"


viewModal : Modal -> Element Msg
viewModal modalState =
    let
        content : List (Element Msg)
        content =
            case modalState of
                LifepathModal state ->
                    viewLifepathModal state

                HealthQuestionModal ( stats, answers ) ->
                    viewHealthQuestionModal stats answers

                SteelQuestionModal ( stats, answers ) ->
                    viewSteelQuestionModal stats answers
    in
    column
        [ width (fill |> minimum 600)
        , height (fill |> maximum 700)
        , Background.color Colors.white
        , Border.rounded 8
        ]
        content


viewSteelQuestionModal : ShadedStats -> Steel.Answers -> List (Element Msg)
viewSteelQuestionModal stats answers =
    let
        viewModifierQuestion : Steel.ModifierView -> Element Msg
        viewModifierQuestion modifier =
            let
                label =
                    paragraph [ width (fill |> maximum 600) ] [ text modifier.question ]
            in
            case modifier.update of
                Just onChange ->
                    Components.questionCheckbox
                        { label = label
                        , onChange = onChange
                        , checked = Common.isJust modifier.value
                        , updateMsg = UpdateSteelAnswers
                        }

                Nothing ->
                    Components.disabledCheckbox
                        { label = label
                        , checked = Common.isJust modifier.value
                        , noop = NoOp
                        }

        viewModifierValue : Steel.ModifierView -> Element Msg
        viewModifierValue modifier =
            let
                val =
                    Maybe.withDefault 0 modifier.value

                plus =
                    if val >= 0 then
                        "+"

                    else
                        ""
            in
            el [ centerX, centerY ] <| text <| plus ++ String.fromInt val
    in
    [ heading "Steel Questions"
    , el [ scrollbarY, width fill, height fill ] <|
        table [ padding 40, spacing 20, width fill, height fill ]
            { data = List.map (Steel.viewModifier ( stats, answers )) Steel.modifiers
            , columns =
                [ { header = none
                  , width = shrink
                  , view = viewModifierQuestion
                  }
                , { header = none
                  , width = shrink
                  , view = viewModifierValue
                  }
                ]
            }
    , modalFooter
        { onSubmit = SubmitSteelModal (Just answers)
        , onCancel = SubmitSteelModal Nothing
        , summary =
            Just <|
                modifierSummary
                    { base = String.fromInt Steel.base
                    , mod = String.fromInt <| Steel.modifier stats answers
                    , total = String.fromInt <| Steel.value stats answers
                    }
        , submitWord = "Submit"
        }
    ]


viewHealthQuestionModal : ShadedStats -> Health.Answers -> List (Element Msg)
viewHealthQuestionModal stats answers =
    let
        viewModifierQuestion : Health.ModifierView -> Element Msg
        viewModifierQuestion modifier =
            let
                label : Element msg
                label =
                    paragraph [ width (fill |> maximum 600) ] [ text modifier.question ]
            in
            case modifier.update of
                Just onChange ->
                    Components.questionCheckbox
                        { label = label
                        , onChange = onChange
                        , checked = Common.isJust modifier.value
                        , updateMsg = UpdateHealthAnswers
                        }

                Nothing ->
                    Components.disabledCheckbox
                        { label = label
                        , checked = Common.isJust modifier.value
                        , noop = NoOp
                        }

        ( shade, base ) =
            Health.base stats

        mod =
            Health.modifier answers

        current =
            Health.compute stats answers |> Tuple.second

        summary =
            modifierSummary
                { base = Shade.toString shade ++ String.fromInt base
                , mod = String.fromInt mod
                , total = Shade.toString shade ++ String.fromInt current
                }

        viewModifierValue : Health.ModifierView -> Element Msg
        viewModifierValue modifier =
            let
                val =
                    Maybe.withDefault 0 modifier.value

                plus =
                    if val >= 0 then
                        "+"

                    else
                        ""
            in
            el [ centerX, centerY ] <| text <| plus ++ String.fromInt val
    in
    [ heading "Health Questions"
    , table [ padding 40, spacing 20, width fill, height fill ] <|
        { data = List.map (Health.viewModifier answers) Health.modifiers
        , columns =
            [ { header = none
              , width = shrink
              , view = viewModifierQuestion
              }
            , { header = none
              , width = shrink
              , view = viewModifierValue
              }
            ]
        }
    , modalFooter
        { onSubmit = SubmitHealthModal (Just answers)
        , onCancel = SubmitHealthModal Nothing
        , summary = Just summary
        , submitWord = "Submit"
        }
    ]


modifierSummary : { base : String, mod : String, total : String } -> String
modifierSummary { base, mod, total } =
    "Base " ++ base ++ " + Modifier " ++ mod ++ " = " ++ total


viewLifepathModal : LifepathModalState -> List (Element Msg)
viewLifepathModal modalState =
    let
        selectedLifepath : Maybe Lifepath
        selectedLifepath =
            List.drop modalState.selectedLifepath modalState.filteredPaths
                |> List.head

        submission : Maybe ( LifepathModalOption, Lifepath )
        submission =
            Maybe.map (Tuple.pair modalState.option) selectedLifepath
    in
    [ heading "Add Lifepath"
    , column [ padding 40 ]
        [ Input.text
            [ htmlAttribute <| Html.Attributes.id modalSearchId
            , Common.onEnter <| SubmitLifepathModal submission
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
                        [ width fill, Events.onDoubleClick <| SubmitLifepathModal submission ]

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
    , modalFooter
        { onSubmit = SubmitLifepathModal submission
        , onCancel = SubmitLifepathModal Nothing
        , summary = Nothing
        , submitWord = "Add"
        }
    ]


type alias FooterOpts msg =
    { onCancel : msg
    , onSubmit : msg
    , summary : Maybe String
    , submitWord : String
    }


modalFooter : FooterOpts msg -> Element msg
modalFooter { onCancel, onSubmit, summary, submitWord } =
    row
        [ height fill
        , width fill
        , Border.rounded 8
        , Background.color Colors.white
        , spacing 20
        , padding 20
        , alignRight
        , alignBottom
        ]
        [ el [ alignLeft ] <| Maybe.withDefault none <| Maybe.map text summary
        , el [ alignRight ] <| Components.faintButton "Cancel" onCancel
        , el [ alignRight ] <| Components.faintButton submitWord onSubmit
        ]


ghostView : DnDList.Model -> List PathWithWarnings -> Element Msg
ghostView dnd lifepaths =
    let
        draggedPath : Maybe PathWithWarnings
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
            let
                overridingStyles : List (Attribute msg)
                overridingStyles =
                    [ Background.color Colors.white
                    , Border.color Colors.faint
                    , Border.rounded 8
                    , Border.width 1
                    , width shrink
                    , height (fill |> maximum 50)
                    , padding 20
                    , htmlAttribute <| Html.Attributes.style "width" "auto"
                    ]
            in
            row (List.map htmlAttribute (system.ghostStyles dnd) ++ overridingStyles) <|
                [ Components.dragHandle []
                , text <| toTitleCase lifepath.name
                , el [ Font.size 18, paddingEach { edges | left = 20 } ] <|
                    (text <| "(" ++ toTitleCase lifepath.settingName ++ ")")
                ]

        Nothing ->
            none


topHeading : String -> Element Msg
topHeading head =
    let
        attrs : List (Attribute Msg)
        attrs =
            Border.roundEach { corners | topLeft = 8, topRight = 8 }
                :: headingAttrs
    in
    el attrs <| text head


heading : String -> Element Msg
heading head =
    el headingAttrs <| text head


headingAttrs : List (Attribute msg)
headingAttrs =
    [ width fill, padding 20, Background.color Colors.faint ]


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


viewDraggableLifepath : DnDList.Model -> Int -> PathWithWarnings -> Element Msg
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
