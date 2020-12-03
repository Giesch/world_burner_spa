port module DnD.DragState exposing
    ( DragData
    , DragState(..)
    , DraggedItem
    , IdDecoders
    , PoisedState
    , Transition(..)
    , subscriptions
    )

import Geom exposing (Box, Point)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)


{-| The possible drag and hover states of the ui.

  - None - not dragging or hovering over anything
  - Hovered - not dragging, and hovering over something of interest
  - Dragged - dragging an item, and not over a meaningful drop
  - Poised - dragging an item, and over a meaningful drop

The ids represent the location of the relevant beacons.
The cache is the carried item and relevant state for dropping it.

-}
type DragState dragId dropId hoverId cache
    = None
    | Hovered hoverId
    | Dragged ( DraggedItem dragId, cache )
    | Poised ( PoisedState dragId dropId, cache )


type alias DraggedItem dragId =
    { beaconId : dragId
    , cursorOnScreen : Point
    , cursorOnDraggable : Point
    }


type alias PoisedState dragId dropId =
    { draggedItem : DraggedItem dragId
    , hoveredDropBeacon : dropId
    }


{-| Transitions are the external message type

  - BeginHover - begins a non-dragging hover
  - EndHover - ends a non-dragging hover
  - PickUp - the beginning of a drag
  - LetGo - the end of a drag (when non-hovering)
  - Drop - the end of a drag (when hovering)
  - Carry - any movement when in either Dragged or Poised states, including transitions
  - NoOp - this represents both NoOps and invalid transitions

-}
type Transition dragId dropId hoverId cache
    = PickUp (DraggedItem dragId)
    | LetGo
    | Drop
    | Carry (DragState dragId dropId hoverId cache)
    | BeginHover hoverId
    | EndHover
    | NoOp


{-| Internal messages received from the port
See draggable.js
-}
type Msg dragId dropId hoverId
    = Start (DraggedItem dragId)
    | Move (DragData dropId)
    | Stop (DragData dropId)
    | Hover (DragData hoverId)
    | DecodeError String


type alias DragData dropId =
    { cursor : Point
    , beacons : List (BeaconBox dropId)
    }


type alias BeaconBox dropId =
    { beaconId : dropId
    , box : Box
    }



-- SUBSCRIPTIONS


port dragEvents : (Decode.Value -> msg) -> Sub msg


subscriptions :
    IdDecoders dragId dropId hoverId
    -> DragState dragId dropId hoverId cache
    -> Sub (Transition dragId dropId hoverId cache)
subscriptions idDecoders dragState =
    Sub.map (transitions dragState) <|
        Sub.batch [ dragEvents (decodeDragEvents idDecoders) ]



-- TRANSITIONS & MOVEMENT


{-| Converts port messages to drag state transitions
-}
transitions :
    DragState dragId dropId hoverId cache
    -> Msg dragId dropId hoverId
    -> Transition dragId dropId hoverId cache
transitions dragState beaconMsg =
    case ( dragState, beaconMsg ) of
        ( None, Hover data ) ->
            case boundingBeaconId data of
                Just id ->
                    BeginHover id

                Nothing ->
                    NoOp

        ( Hovered hovered, Hover data ) ->
            case boundingBeaconId data of
                Just id ->
                    if id == hovered then
                        NoOp

                    else
                        BeginHover id

                Nothing ->
                    EndHover

        ( Hovered _, Start draggedItem ) ->
            PickUp draggedItem

        ( Hovered _, _ ) ->
            -- these are error states
            EndHover

        ( None, Start draggedItem ) ->
            PickUp draggedItem

        ( None, _ ) ->
            NoOp

        ( Dragged pair, Move data ) ->
            Carry <| carryState data pair

        ( Dragged _, Stop _ ) ->
            LetGo

        ( Dragged _, _ ) ->
            NoOp

        ( Poised ( { draggedItem }, cache ), Move data ) ->
            Carry <| carryState data ( draggedItem, cache )

        ( Poised _, Stop _ ) ->
            Drop

        ( Poised _, _ ) ->
            NoOp


carryState :
    DragData dropId
    -> ( DraggedItem dragId, cache )
    -> DragState dragId dropId hoverId cache
carryState data ( draggedItem, cache ) =
    let
        move : DraggedItem dragId -> DraggedItem dragId
        move item =
            { item | cursorOnScreen = data.cursor }
    in
    case boundingBeaconId data of
        Nothing ->
            Dragged <| ( move draggedItem, cache )

        Just dropBeaconId ->
            Poised <| ( PoisedState (move draggedItem) dropBeaconId, cache )


boundingBeaconId : DragData id -> Maybe id
boundingBeaconId data =
    boundingBeacons data
        |> topBeacon
        |> Maybe.map .beaconId


boundingBeacons : DragData id -> DragData id
boundingBeacons data =
    { cursor = data.cursor
    , beacons =
        List.filter
            (\beacon -> Geom.bounds beacon.box data.cursor)
            data.beacons
    }


topBeacon : DragData id -> Maybe (BeaconBox id)
topBeacon { beacons } =
    -- NOTE this relies on 2 things
    -- 1. Document.querySelectorAll always returns elements in document order
    -- 2. In this app, document order is ascending z index order for 'auto' z indexes.
    --    eg, we always stack cards left to right
    List.head <| List.reverse beacons



-- DECODERS


decodeDragEvents :
    IdDecoders dragId dropId hoverId
    -> Decode.Value
    -> Msg dragId dropId hoverId
decodeDragEvents idDecoders value =
    case Decode.decodeValue (msgDecoder idDecoders) value of
        Ok msg ->
            msg

        Err err ->
            -- TODO do something useful with this
            DecodeError <| Decode.errorToString err


msgDecoder : IdDecoders dragId dropId hoverId -> Decode.Decoder (Msg dragId dropId hoverId)
msgDecoder idDecoders =
    Decode.succeed BeaconJson
        |> required "type" eventDecoder
        |> required "cursor" Geom.pointDecoder
        |> required "beacons" beaconsDecoder
        |> optional "startBeaconId" (Decode.map Just Decode.string) Nothing
        |> optional "cursorOnDraggable" (Decode.map Just Geom.pointDecoder) Nothing
        |> Decode.andThen (dragEvent idDecoders)


type alias BeaconJson =
    { eventType : EventType
    , cursor : Point
    , beacons : List JSBeacon
    , startBeaconId : Maybe String
    , cursorOnDraggable : Maybe Point
    }


type alias JSBeacon =
    { id : Int
    , box : Box
    }


type EventType
    = StartEvent
    | MoveEvent
    | StopEvent
    | HoverEvent


eventDecoder : Decode.Decoder EventType
eventDecoder =
    Decode.string
        |> Decode.andThen
            (\eventType ->
                case eventType of
                    "start" ->
                        Decode.succeed StartEvent

                    "move" ->
                        Decode.succeed MoveEvent

                    "stop" ->
                        Decode.succeed StopEvent

                    "hover" ->
                        Decode.succeed HoverEvent

                    _ ->
                        Decode.fail ("Unknown drag event type " ++ eventType)
            )


beaconsDecoder : Decode.Decoder (List JSBeacon)
beaconsDecoder =
    Decode.list <|
        Decode.map2 JSBeacon
            (Decode.field "id" Decode.int)
            Geom.boxDecoder


type alias IdDecoders dragId dropId hoverId =
    { toDragId : Int -> dragId
    , toDropId : Int -> dropId
    , toHoverId : Int -> hoverId
    }


dragEvent :
    IdDecoders dragId dropId hoverId
    -> BeaconJson
    -> Decode.Decoder (Msg dragId dropId hoverId)
dragEvent { toDragId, toDropId, toHoverId } json =
    let
        dropData : DragData dropId
        dropData =
            dragData toDropId json
    in
    case json.eventType of
        HoverEvent ->
            Decode.succeed <| Hover <| dragData toHoverId json

        StartEvent ->
            startEvent toDragId json dropData.cursor

        MoveEvent ->
            Decode.succeed <| Move dropData

        StopEvent ->
            Decode.succeed <| Stop dropData


{-| Converts BeaconJson to DragData, including only beacons of the expected type.
-}
dragData : (Int -> id) -> BeaconJson -> DragData id
dragData toId json =
    let
        convert : JSBeacon -> BeaconBox id
        convert { id, box } =
            BeaconBox (toId id) box
    in
    { cursor = json.cursor
    , beacons = List.map convert json.beacons
    }


startEvent :
    (Int -> dragId)
    -> BeaconJson
    -> Point
    -> Decode.Decoder (Msg dragId dropId hoverId)
startEvent toDragId { startBeaconId, cursorOnDraggable } cursor =
    let
        dragId : Maybe dragId
        dragId =
            startBeaconId
                |> Maybe.andThen String.toInt
                |> Maybe.map toDragId
    in
    case ( dragId, cursorOnDraggable ) of
        ( Just id, Just onDraggable ) ->
            Decode.succeed <| Start <| DraggedItem id cursor onDraggable

        _ ->
            Decode.fail "Received start event with no beacon id"
