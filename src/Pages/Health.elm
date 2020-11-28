module Pages.Health exposing (Model, Msg, Params, page)

import Api
import Element exposing (..)
import Http
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
    { health : Maybe (Result Http.Error ()) }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { health = Nothing }
    , Api.healthCheck HealthCheck
    )



-- UPDATE


type Msg
    = HealthCheck (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HealthCheck health ->
            ( { model | health = Just health }, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Health"
    , body =
        [ case model.health of
            Nothing ->
                text "loading"

            Just (Ok _) ->
                text "Healthy!"

            Just (Err err) ->
                text "Oh No!"
        ]
    }
