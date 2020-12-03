module DnD exposing
    ( DragKind(..)
    , HoverableOptions
    , draggable
    , hoverable
    )

{-| Higher order view functions for hoverable, draggable, and droppable items
-}

import DnD.Beacon as Beacon
import Element exposing (..)
import Geom exposing (Point)
import Html
import Html.Attributes


type alias HoverableOptions msg =
    { hovered : Bool
    , attrs : List (Attribute msg)
    , viewUnhovered : Element msg
    , viewHovered : Element msg
    , location : Beacon.HoverBeaconLocation
    }


{-| An element that changes when hovered with an empty cursor.
-}
hoverable : HoverableOptions msg -> Element msg
hoverable { hovered, attrs, viewUnhovered, viewHovered, location } =
    let
        beacon : Attribute msg
        beacon =
            Beacon.hoverBeacon location
    in
    el (beacon :: attrs) <|
        if hovered then
            viewHovered

        else
            viewUnhovered


type alias DragDisplay =
    { cursorOnScreen : Point
    , cursorOnDraggable : Point
    }


type alias DraggableViews msg =
    { viewUnhovered : Element msg
    , viewHovered : Element msg
    , viewDragged : Element msg
    , viewDragSource : Element msg
    }


type alias DraggableOptions msg =
    { attrs : List (Attribute msg)
    , views : DraggableViews msg
    , hoverLocation : Beacon.HoverBeaconLocation
    , dragLocation : Beacon.DragBeaconLocation
    , dragKind : DragKind
    }


type DragKind
    = None
    | Hovered
    | Dragged DragDisplay


{-| An element that changes when hovered with an empty cursor, and can be picked up.
-}
draggable : DraggableOptions msg -> Element msg
draggable opts =
    let
        attrs : List (Attribute msg)
        attrs =
            beacons ++ opts.attrs

        beacons : List (Attribute msg)
        beacons =
            [ Beacon.hoverBeacon opts.hoverLocation
            , Beacon.dragBeacon opts.dragLocation
            ]
    in
    case opts.dragKind of
        None ->
            el attrs opts.views.viewUnhovered

        Hovered ->
            el attrs opts.views.viewHovered

        Dragged { cursorOnScreen, cursorOnDraggable } ->
            let
                floatingAttrs =
                    [ htmlAttribute <| Html.Attributes.style "position" "fixed"
                    , htmlAttribute <| position "top" <| cursorOnScreen.y - cursorOnDraggable.y
                    , htmlAttribute <| position "left" <| cursorOnScreen.x - cursorOnDraggable.x
                    , htmlAttribute <| Html.Attributes.style "list-style" "none"
                    , htmlAttribute <| Html.Attributes.style "padding" "0"
                    , htmlAttribute <| Html.Attributes.style "margin" "0"
                    , htmlAttribute <| Html.Attributes.style "z-index" "100"
                    ]

                position : String -> Float -> Html.Attribute msg
                position side px =
                    Html.Attributes.style side <| String.fromFloat px ++ "px"
            in
            column opts.attrs
                [ el floatingAttrs opts.views.viewDragged
                , el opts.attrs opts.views.viewDragSource
                ]
