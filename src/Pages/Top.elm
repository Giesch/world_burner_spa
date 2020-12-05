module Pages.Top exposing (Model, Msg, Params, page)

import Api
import Browser.Dom
import Browser.Events
import Colors
import Common exposing (edges)
import Components
import DnDList
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon
import FontAwesome.Solid
import Html
import Html.Attributes
import Http
import Json.Decode as Decode
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Lifepath.GenSkills as GenSkills exposing (GenSkills)
import Model.Lifepath.Resources as Resources exposing (Resources)
import Model.Lifepath.StatMod as StatMod exposing (StatMod)
import Model.Lifepath.Trait as Trait exposing (Trait)
import Model.Lifepath.Validation as Validation
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
    { allLifepaths : Status (List Lifepath)
    , name : String
    , lifepaths : List Lifepath
    , dnd : DnDList.Model
    , modalState : Maybe ModalState
    }


type alias ModalState =
    { searchText : String
    , filteredPaths : List Lifepath
    , allLifepaths : List Lifepath
    , selectedLifepath : Int
    }


defaultModal : List Lifepath -> ModalState
defaultModal allLifepaths =
    { searchText = ""
    , filteredPaths = allLifepaths
    , allLifepaths = allLifepaths
    , selectedLifepath = 0
    }


config : DnDList.Config Lifepath
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system : DnDList.System Lifepath Msg
system =
    DnDList.create config DnDMsg


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { allLifepaths = Status.Loading
      , name = ""
      , lifepaths = []
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
            ( { model | allLifepaths = Status.Loaded allLifepaths }, Cmd.none )

        GotLifepaths (Err _) ->
            ( { model | allLifepaths = Status.Failed }, Cmd.none )

        EnteredName name ->
            ( { model | name = name }, Cmd.none )

        DnDMsg dndMsg ->
            let
                ( dnd, lifepaths ) =
                    system.update dndMsg model.dnd model.lifepaths
            in
            ( { model | dnd = dnd, lifepaths = lifepaths }
            , system.commands dnd
            )

        RemoveLifepath index ->
            let
                lifepaths =
                    List.take index model.lifepaths
                        ++ List.drop (index + 1) model.lifepaths
            in
            ( { model | lifepaths = lifepaths }, Cmd.none )

        OpenModal ->
            case model.allLifepaths of
                Status.Loaded allLifepaths ->
                    ( { model | modalState = Just <| defaultModal allLifepaths }
                    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus modalSearchId
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitModal Nothing ->
            ( { model | modalState = Nothing }, Cmd.none )

        SubmitModal (Just addedLifepath) ->
            ( { model
                | modalState = Nothing
                , lifepaths = model.lifepaths ++ [ addedLifepath ]
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
    case direction of
        Up ->
            let
                selectedLifepath =
                    modalState.selectedLifepath - 1
            in
            ( { modalState | selectedLifepath = max selectedLifepath 0 }
            , Cmd.none
            )

        Down ->
            let
                selectedLifepath =
                    modalState.selectedLifepath + 1
            in
            ( { modalState | selectedLifepath = min selectedLifepath <| List.length modalState.filteredPaths }
            , Cmd.none
            )

        Other ->
            ( modalState, Cmd.none )


beginSearchDebounce : String -> Cmd Msg
beginSearchDebounce searchText =
    Process.sleep 250
        |> Task.perform (\_ -> SearchTimePassed searchText)


searchTimePassed : String -> ModalState -> ModalState
searchTimePassed oldSearchText modalState =
    if oldSearchText == modalState.searchText then
        filterLifepaths oldSearchText modalState

    else
        modalState


filterLifepaths : String -> ModalState -> ModalState
filterLifepaths searchText modalState =
    let
        filteredPaths =
            List.filter
                (\lp ->
                    List.any (\field -> String.contains searchText field) lp.searchContent
                )
                modalState.allLifepaths
    in
    { modalState
        | selectedLifepath = 0
        , searchText = searchText
        , filteredPaths = filteredPaths
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
            , text <| "Age: " ++ String.fromInt (calculateAge model.lifepaths)
            ]
        , heading "Lifepaths"
        , viewCharacterLifepaths model
        , el [ paddingEach { edges | right = 20, bottom = 20 }, alignRight ] <| faintButton "Add Lifepath" (Just OpenModal)
        , ghostView model.dnd model.lifepaths
        ]
    }


calculateAge : List Lifepath -> Int
calculateAge lifepaths =
    let
        years lp =
            case lp.years of
                Years.Count yrs ->
                    yrs

                Years.Range ( _, max ) ->
                    max
    in
    List.sum <| List.map years lifepaths


viewCharacterLifepaths : Model -> Element Msg
viewCharacterLifepaths model =
    el [ width fill, padding 20 ] <|
        case model.lifepaths of
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
                            (\i ( lifepath, warnings ) ->
                                viewDraggableLifepath
                                    { dnd = model.dnd
                                    , maybeIndex = Just i
                                    , lifepath = lifepath
                                    , warnings = warnings
                                    }
                            )
                            (Validation.validate lifepaths)


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


ghostView : DnDList.Model -> List Lifepath -> Element Msg
ghostView dnd lifepaths =
    let
        maybePath =
            system.info dnd
                |> Maybe.andThen
                    (\{ dragIndex } ->
                        lifepaths |> List.drop dragIndex |> List.head
                    )
    in
    case maybePath of
        Just path ->
            el
                (Background.color Colors.white
                    :: Border.color Colors.faint
                    :: Border.rounded 8
                    :: Border.widthXY 0 1
                    :: (List.map htmlAttribute <| system.ghostStyles dnd)
                )
            <|
                viewDraggableLifepath { dnd = dnd, maybeIndex = Nothing, lifepath = path, warnings = [] }

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
        }


type alias InnerLifepathOptions =
    { dndStyles : List (Attribute Msg)
    , lifepath : Lifepath
    , id : String
    , removeBtnIndex : Maybe (Maybe Int)
    , warnings : List String
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

        rowAttrs =
            baseRowAttrs ++ opts.dndStyles
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
                , paragraph [ width fill ] <|
                    [ text "Leads: "
                    , text <| String.join ", " <| List.map (.settingName >> toTitleCase) opts.lifepath.leads
                    ]
                , paragraph [] <|
                    [ text <| "Skills: "
                    , text <|
                        String.fromInt opts.lifepath.skillPts
                            ++ " pts: "
                            ++ (String.join ", " <|
                                    List.map (.displayName >> nonBreakingHyphens >> toTitleCase)
                                        opts.lifepath.skills
                               )
                    , el [ alignLeft ] none
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
                    , label = lifepathIcon FontAwesome.Solid.trash
                    }
        ]


warningIcon : List String -> Element msg
warningIcon warnings =
    let
        tooltip : Attribute msg
        tooltip =
            Components.tooltip onLeft (warningsTooltip warnings)
    in
    el
        [ alignRight
        , alignTop
        , tooltip
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
        }


nonBreakingHyphens : String -> String
nonBreakingHyphens =
    String.map <|
        \c ->
            if c == '-' then
                Char.fromCode 8209

            else
                c
