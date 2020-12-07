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
import Element.Border as Border
import Element.Events as Events
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
    , modalOpen : Bool
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model url key False
    , Cmd.none
    )



-- UPDATE


type Msg
    = CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseModal ->
            ( { model | modalOpen = False }, Cmd.none )


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
    , modal = Maybe.map (viewModal toMsg) page.modal
    , body =
        let
            routeLink route label =
                link [ Font.color Colors.black, Font.underline ]
                    { url = Route.toString route, label = text label }
        in
        [ column
            [ height fill, width fill ]
            [ row
                [ padding 20
                , spacing 20
                , height (fill |> maximum 60)
                , width fill
                , Background.color Colors.faint
                ]
                []
            , row
                [ height fill
                , width (fill |> maximum 1600)
                , paddingXY 100 50
                , centerX
                ]
                [ column
                    [ height fill
                    , width fill
                    , Border.width 1
                    , Border.rounded 8
                    , Border.color Colors.faint
                    ]
                    page.body
                ]
            ]
        ]
    }


viewModal : (Msg -> msg) -> Element msg -> Element msg
viewModal toMsg content =
    column [ width fill, height fill ]
        [ backdrop (px 100) toMsg
        , row [ width fill ]
            [ backdrop fill toMsg
            , el
                [ width fill
                , height fill
                , Border.rounded 8
                , Border.shadow
                    { offset = ( 5, 5 )
                    , blur = 6
                    , size = 0
                    , color = Colors.darkShadow
                    }
                , behindContent <| backdrop fill toMsg
                ]
                content
            , backdrop fill toMsg
            ]
        , backdrop (fill |> minimum 100) toMsg
        ]


backdrop : Element.Length -> (Msg -> msg) -> Element msg
backdrop h toMsg =
    el
        [ width fill
        , height h
        , Background.color Colors.darkened
        , Events.onClick (toMsg CloseModal)
        ]
        none
