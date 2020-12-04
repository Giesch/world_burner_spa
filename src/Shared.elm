module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation exposing (Key)
import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Url exposing (Url)



-- INIT


type alias Flags =
    ()


type alias Model =
    { url : Url
    , key : Key
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model url key
    , Cmd.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    { page : Document msg, toMsg : Msg -> msg }
    -> Model
    -> Document msg
view { page, toMsg } model =
    { title = page.title
    , body =
        let
            routeLink route label =
                link [ Font.color Colors.white, Font.underline ]
                    { url = Route.toString route, label = text label }
        in
        [ column
            [ height fill, width fill, scrollbarY ]
            [ row
                [ padding 20
                , spacing 20
                , height (fill |> maximum 60)
                , width fill
                , Background.color Colors.darkened
                ]
                [ routeLink Route.Top "Homepage"
                , routeLink Route.Create "Create"
                , routeLink Route.NotFound "Not Found"
                ]
            , column [ height fill, width fill, scrollbarY ] page.body
            ]
        ]
    }
