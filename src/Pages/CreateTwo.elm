module Pages.CreateTwo exposing (Model, Msg, Params, page)

import Api
import Array exposing (Array)
import Colors
import Common
import Components.LifepathFilter as LifepathFilter exposing (LifepathFilter)
import DnDList
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes
import Http
import Model.Lifepath as Lifepath exposing (Lifepath)
import Model.Status as Status exposing (Status)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)


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
    { searchFilter : LifepathFilter
    , sidebarLifepaths : Status LoadedLifepaths
    , dnd : DnDList.Model
    }


type alias LoadedLifepaths =
    { all : Array Lifepath
    , sidebar : Array Lifepath
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { searchFilter = LifepathFilter.none
      , sidebarLifepaths = Status.Loading
      , dnd = system.model
      }
    , Api.dwarves GotDwarves
    )


config : DnDList.Config Lifepath
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrop
    , operation = DnDList.Unaltered
    }


system : DnDList.System Lifepath Msg
system =
    DnDList.create config DnD



-- UPDATE


type Msg
    = GotDwarves (Result Http.Error (List Lifepath))
    | DnD DnDList.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DnD m ->
            let
                ( dnd, lifepaths ) =
                    system.update m model.dnd <| loadedLifepaths model.sidebarLifepaths

                -- TODO handle lifepath list with Status.map
            in
            ( { model | dnd = dnd }, system.commands dnd )

        GotDwarves (Ok lifepaths) ->
            let
                all =
                    Array.fromList lifepaths

                sidebarLifepaths =
                    Status.Loaded { all = all, sidebar = all }
            in
            ( { model | sidebarLifepaths = sidebarLifepaths }
            , Cmd.none
            )

        GotDwarves (Err _) ->
            ( { model | sidebarLifepaths = Status.Failed }
            , Cmd.none
            )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd



-- VIEW


view : Model -> Document Msg
view model =
    { title = "CreateTwo"
    , body =
        [ row
            ([ width fill
             , height fill
             , scrollbarY
             , spacing 40
             ]
                ++ Common.scrollbarsFix
            )
            [ viewSidebar model
            , ghostView model.dnd model.sidebarLifepaths
            ]
        ]
    }


viewSidebar : Model -> Element Msg
viewSidebar model =
    column
        [ width <| px 350
        , height fill
        , scrollbarY
        , Background.color Colors.darkened
        , Font.color Colors.white
        , spacing 20
        ]
        [ LifepathFilter.view
            { enteredSearchText = \_ -> NoOp
            , clearFit = NoOp
            , clearFix = NoOp
            }
            model.searchFilter
        , viewSidebarLifepaths model.sidebarLifepaths
        ]


viewSidebarLifepaths : Status LoadedLifepaths -> Element Msg
viewSidebarLifepaths sidebarLifepaths =
    let
        viewPath : Int -> Lifepath -> Element Msg
        viewPath index path =
            let
                id =
                    "sidebar-" ++ String.fromInt index
            in
            el
                (List.map htmlAttribute
                    (Html.Attributes.id id :: system.dragEvents index id)
                )
            <|
                Lifepath.view Nothing path
    in
    case sidebarLifepaths of
        Status.Loading ->
            text "loading..."

        Status.Failed ->
            text "couldn't load lifepaths"

        Status.Loaded { sidebar } ->
            column [ spacing 20, padding 20, width fill, height fill, scrollbarY ] <|
                List.indexedMap viewPath <|
                    Array.toList sidebar


ghostView : DnDList.Model -> Status LoadedLifepaths -> Element Msg
ghostView dnd status =
    let
        lifepaths : List Lifepath
        lifepaths =
            loadedLifepaths status

        maybeDragPath : Maybe Lifepath
        maybeDragPath =
            system.info dnd
                |> Maybe.andThen
                    (\{ dragIndex } ->
                        lifepaths |> List.drop dragIndex |> List.head
                    )
    in
    case maybeDragPath of
        Just path ->
            el (List.map htmlAttribute <| system.ghostStyles dnd) <|
                Lifepath.view Nothing path

        Nothing ->
            none


loadedLifepaths : Status LoadedLifepaths -> List Lifepath
loadedLifepaths status =
    case status of
        Status.Loaded { all } ->
            Array.toList all

        _ ->
            []
