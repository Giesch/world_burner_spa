module Components.Workbench exposing
    ( DropError(..)
    , Hover(..)
    , PickupError(..)
    , Workbench
    , WorkbenchOptions
    , default
    , deleteBlock
    , drop
    , pickup
    , view
    , viewDraggedBlock
    )

import Array exposing (Array)
import Colors
import Common
import DnD.Beacon as Beacon
    exposing
        ( BenchIndex
        , BenchLocation
        , DropBeaconLocation
        , HoverBeaconLocation
        )
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Model.LifeBlock as LifeBlock exposing (LifeBlock, SplitResult(..))
import Model.LifeBlock.Validation as Validation
import Model.Lifepath as Lifepath


type Workbench
    = Workbench (Array LifeBlock)


maxLength : Int
maxLength =
    4


default : Workbench
default =
    Workbench <| Array.empty


deleteBlock : Workbench -> BenchIndex -> Workbench
deleteBlock (Workbench bench) benchIndex =
    Workbench <|
        Array.append
            (Array.slice 0 benchIndex bench)
            (Array.slice (benchIndex + 1) (maxLength + 1) bench)


type PickupError
    = PickupBoundsError
    | NoLifeBlock


{-| Returns the workbench with the the picked up block removed, and the picked up block.
However, this is not applied immediately; only once the user releases the drop.
-}
pickup : Workbench -> BenchLocation -> Result PickupError ( Workbench, LifeBlock )
pickup (Workbench bench) { benchIndex, blockIndex } =
    case Array.get benchIndex bench of
        Nothing ->
            Err PickupBoundsError

        Just block ->
            case LifeBlock.splitAt blockIndex block of
                Whole pickedup ->
                    Ok ( deleteBlock (Workbench bench) benchIndex, pickedup )

                Split ( left, right ) ->
                    Ok ( putBenchBlock benchIndex left bench, right )

                BoundsError ->
                    Err PickupBoundsError


type DropError
    = DropBoundsError
    | InvalidDropLocation
    | CombinationError (NonEmpty Validation.Error)


type alias CombinationResult =
    Result (NonEmpty Validation.Error) LifeBlock


drop : Workbench -> LifeBlock -> DropBeaconLocation -> Result DropError Workbench
drop (Workbench bench) droppedBlock location =
    let
        dropFinal : LifeBlock -> BenchIndex -> Result DropError Workbench
        dropFinal finalBlock benchIndex =
            Ok <| putBenchBlock benchIndex finalBlock bench

        combineAndDrop : Int -> (LifeBlock -> CombinationResult) -> Result DropError Workbench
        combineAndDrop benchIndex combineWithBenchBlock =
            case Array.get benchIndex bench of
                Nothing ->
                    Err DropBoundsError

                Just benchBlock ->
                    case combineWithBenchBlock benchBlock of
                        Ok combined ->
                            dropFinal combined benchIndex

                        Err err ->
                            Err <| CombinationError err
    in
    case location of
        Beacon.OpenSlot pos ->
            case pos of
                Beacon.BeforeBench ->
                    let
                        newBench : Array LifeBlock
                        newBench =
                            Array.append (Array.fromList [ droppedBlock ]) bench
                    in
                    Ok <| Workbench <| newBench

                Beacon.AfterBench ->
                    Ok <| Workbench <| Array.push droppedBlock bench

        Beacon.BeforeSlot idx ->
            combineAndDrop idx <|
                \benchBlock -> LifeBlock.combine droppedBlock benchBlock

        Beacon.AfterSlot idx ->
            combineAndDrop idx <|
                \benchBlock -> LifeBlock.combine benchBlock droppedBlock


putBenchBlock : BenchIndex -> LifeBlock -> Array LifeBlock -> Workbench
putBenchBlock index maybeBlock bench =
    Workbench <| Array.set index maybeBlock bench


type Hover
    = None
    | Hovered HoverBeaconLocation
    | Dragged LifeBlock
    | Poised Poise


type alias Poise =
    { hoveringBlock : LifeBlock
    , dropLocation : DropBeaconLocation
    , dropHighlight : Maybe Bool
    }


type alias WorkbenchOptions msg =
    { hover : Hover
    , deleteBenchBlock : BenchIndex -> msg
    , filterPressed : LifeBlock.Fit -> msg
    , setFix : NonEmpty Validation.WarningReason -> msg
    }


view : Workbench -> WorkbenchOptions msg -> Element msg
view (Workbench slots) opts =
    let
        blockHover : Int -> LifeBlock.Hover
        blockHover benchIndex =
            case opts.hover of
                Hovered (Beacon.LifeBlockWarning loc) ->
                    if loc.benchIndex == benchIndex then
                        LifeBlock.Warning loc.warningIndex

                    else
                        LifeBlock.None

                Hovered (Beacon.HoverBefore i) ->
                    if i == benchIndex then
                        LifeBlock.FilterButton LifeBlock.Before

                    else
                        LifeBlock.None

                Hovered (Beacon.HoverAfter i) ->
                    if i == benchIndex then
                        LifeBlock.FilterButton LifeBlock.After

                    else
                        LifeBlock.None

                Dragged _ ->
                    LifeBlock.Carry Nothing

                Poised full ->
                    convertHighlight benchIndex full

                None ->
                    LifeBlock.None

        viewBlock : Int -> LifeBlock -> Element msg
        viewBlock benchIndex =
            viewLifeBlock
                { benchIndex = benchIndex
                , deleteBenchBlock = opts.deleteBenchBlock
                , hover = blockHover benchIndex
                , filterPressed = opts.filterPressed
                , setFix = opts.setFix
                }

        viewSlot : Int -> LifeBlock -> Element msg
        viewSlot benchIndex block =
            block |> viewBlock benchIndex

        fullSlots =
            Array.indexedMap viewSlot slots
    in
    row
        [ spacing 20
        , padding 40
        , centerX
        , centerY
        , height <| px 500
        , width fill
        ]
    <|
        if Array.isEmpty fullSlots then
            [ openSlot Beacon.AfterBench opts ]

        else if Array.length fullSlots == maxLength then
            Array.toList fullSlots

        else if Array.length fullSlots == maxLength - 1 then
            Array.toList <| Array.push (openSlot Beacon.AfterBench opts) fullSlots

        else
            openSlot Beacon.BeforeBench opts
                :: (Array.toList <|
                        Array.push (openSlot Beacon.AfterBench opts) fullSlots
                   )


convertHighlight : Int -> Poise -> LifeBlock.Hover
convertHighlight benchIndex { dropLocation, dropHighlight } =
    let
        checkIndex : Int -> LifeBlock.Position -> Bool -> LifeBlock.Hover
        checkIndex i position successful =
            if i == benchIndex then
                LifeBlock.Carry (Just ( position, successful ))

            else
                LifeBlock.Carry Nothing
    in
    case ( dropLocation, dropHighlight ) of
        ( Beacon.BeforeSlot i, Just successful ) ->
            checkIndex i LifeBlock.Before successful

        ( Beacon.AfterSlot i, Just successful ) ->
            checkIndex i LifeBlock.After successful

        _ ->
            LifeBlock.Carry Nothing


openSlot : Beacon.OpenSlotPosition -> WorkbenchOptions msg -> Element msg
openSlot position { hover } =
    let
        beingHovered : Bool
        beingHovered =
            case hover of
                Poised full ->
                    full.dropLocation == Beacon.OpenSlot position

                _ ->
                    False
    in
    if beingHovered then
        el
            (Beacon.dropBeacon (Beacon.OpenSlot position)
                :: Colors.successGlow
                :: slotAttrs
            )
            (el [ centerX, centerY ] <| text "+")

    else
        el
            (Beacon.dropBeacon (Beacon.OpenSlot position)
                :: Border.width 1
                :: slotAttrs
            )
            (el [ centerX, centerY ] <| text "+")


type alias DraggedBlockOptions =
    { top : Float
    , left : Float
    , errors : Maybe (NonEmpty Validation.Error)
    }


{-| Displays the hovering block at the users cursor
-}
viewDraggedBlock : LifeBlock -> DraggedBlockOptions -> Element msg
viewDraggedBlock lifeBlock { top, left, errors } =
    let
        position : String -> Float -> Html.Attribute msg
        position name px =
            Html.Attributes.style name <| String.fromFloat px ++ "px"

        errsAttr : Attribute msg
        errsAttr =
            Maybe.map viewErrors errors
                |> Maybe.withDefault none
                |> Element.onLeft

        attrs : List (Attribute msg)
        attrs =
            [ htmlAttribute <| Html.Attributes.style "position" "fixed"
            , htmlAttribute <| position "top" top
            , htmlAttribute <| position "left" left
            , htmlAttribute <| Html.Attributes.style "list-style" "none"
            , htmlAttribute <| Html.Attributes.style "padding" "0"
            , htmlAttribute <| Html.Attributes.style "margin" "0"
            , width Lifepath.lifepathWidth
            , errsAttr
            , spacing 20
            , padding 12
            ]
                ++ Common.userSelectNone
    in
    column attrs <|
        List.map (Lifepath.view Nothing) <|
            (NonEmpty.toList <| LifeBlock.paths lifeBlock)


viewErrors : NonEmpty Validation.Error -> Element msg
viewErrors errs =
    let
        viewErr (Validation.Error msg) =
            text msg
    in
    column [] <| List.map viewErr <| NonEmpty.toList errs


type alias LifeBlockOptions msg =
    { benchIndex : Int
    , deleteBenchBlock : BenchIndex -> msg
    , filterPressed : LifeBlock.Fit -> msg
    , hover : LifeBlock.Hover
    , setFix : NonEmpty Validation.WarningReason -> msg
    }


viewLifeBlock : LifeBlockOptions msg -> LifeBlock -> Element msg
viewLifeBlock opts =
    LifeBlock.view
        { baseAttrs = slotAttrs
        , onDelete = Just <| opts.deleteBenchBlock opts.benchIndex
        , benchIndex = opts.benchIndex
        , hover = opts.hover
        , filterPressed = opts.filterPressed
        , setFix = opts.setFix
        }


{-| Attributes common to open and filled slots on the bench
-}
slotAttrs : List (Attribute msg)
slotAttrs =
    [ Background.color Colors.white
    , Font.color Colors.black
    , Border.rounded 8
    , Border.color Colors.darkened
    , width <| px 350
    , height fill
    , spacing 20
    , padding 12
    , centerX
    , centerY
    ]
