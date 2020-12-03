module DnD.Beacon exposing
    ( BenchIndex
    , BenchLocation
    , DragBeaconId
    , DragBeaconLocation(..)
    , DropBeaconId
    , DropBeaconLocation(..)
    , HoverBeaconId
    , HoverBeaconLocation(..)
    , OpenSlotPosition(..)
    , WarningLocation
    , decoders
    , dragBeacon
    , dragLocation
    , dropBeacon
    , dropLocation
    , fromDragId
    , fromDropId
    , fromHoverId
    , hoverBeacon
    , hoverLocation
    , toDragId
    , toDropId
    , toHoverId
    )

import DnD.DragState as DragState
import Element
import Html.Attributes
import Json.Encode as Encode



-- TODO make this a Lifepaths submodule


type alias BenchIndex =
    Int



-- DRAG BEACONS


{-| An opaque and deterministic location-based id for a draggable item
-}
type DragBeaconId
    = DragBeaconId Int


{-| The location of a drag beacon in the Lifepaths page model

  - benchIndex - 0 based index on the workbench (must be less than 10)
  - blockIndex - 0 based index within the lifeblock (must be less than 10)

-}
type DragBeaconLocation
    = Sidebar Int
    | Bench BenchLocation


type alias BenchLocation =
    { benchIndex : BenchIndex
    , blockIndex : Int
    }


dragBeacon : DragBeaconLocation -> Element.Attribute msg
dragBeacon location =
    beaconAttribute "drag-beacon" <| toDragId location


dragLocation : DragBeaconId -> DragBeaconLocation
dragLocation (DragBeaconId id) =
    fromDragId id


toDragId : DragBeaconLocation -> Int
toDragId location =
    case location of
        Sidebar i ->
            -i - 1

        Bench loc ->
            loc.benchIndex * 10 + loc.blockIndex


fromDragId : Int -> DragBeaconLocation
fromDragId id =
    if id < 0 then
        Sidebar <| -id - 1

    else
        Bench
            { benchIndex = tensPlace id
            , blockIndex = modBy 10 id
            }



-- DROP BEACONS


{-| An opaque and deterministic location-based id for a drop location
-}
type DropBeaconId
    = DropBeaconId Int


{-| The location of a drop beacon in the Lifepaths page model.
All indexes are 0 based and must be less than 10.

  - Open - an empty slot on the workbench at the given index
  - Before - the drop area before/above the lifeblock at a given index
  - After - the drop area after/below the lifeblock at a given index

TODO consider replacing this with a pair, or something

-}
type DropBeaconLocation
    = OpenSlot OpenSlotPosition
    | BeforeSlot BenchIndex
    | AfterSlot BenchIndex


type OpenSlotPosition
    = BeforeBench
    | AfterBench


slotPosId : OpenSlotPosition -> Int
slotPosId pos =
    case pos of
        BeforeBench ->
            0

        AfterBench ->
            1


dropBeacon : DropBeaconLocation -> Element.Attribute msg
dropBeacon location =
    beaconAttribute "drop-beacon" <| toDropId location


dropLocation : DropBeaconId -> DropBeaconLocation
dropLocation (DropBeaconId id) =
    fromDropId id


toDropId : DropBeaconLocation -> Int
toDropId location =
    case location of
        OpenSlot pos ->
            (slotPosId pos + 1) * -1

        BeforeSlot benchIndex ->
            (benchIndex + 11) * -1

        AfterSlot benchIndex ->
            (benchIndex + 21) * -1


fromDropId : Int -> DropBeaconLocation
fromDropId id =
    if id == -1 then
        OpenSlot BeforeBench

    else if id == -2 then
        OpenSlot AfterBench

    else if id < -10 && id >= -20 then
        BeforeSlot <| -id - 11

    else
        AfterSlot <| -id - 21



-- HOVER BEACONS


type HoverBeaconId
    = HoverBeaconId Int


type HoverBeaconLocation
    = LifeBlockWarning WarningLocation
    | HoverBefore BenchIndex
    | HoverAfter BenchIndex


type alias WarningLocation =
    { benchIndex : BenchIndex
    , warningIndex : Int
    }


hoverBeacon : HoverBeaconLocation -> Element.Attribute msg
hoverBeacon location =
    beaconAttribute "hover-beacon" <| toHoverId location


hoverLocation : HoverBeaconId -> HoverBeaconLocation
hoverLocation (HoverBeaconId id) =
    fromHoverId id


toHoverId : HoverBeaconLocation -> Int
toHoverId location =
    case location of
        LifeBlockWarning { benchIndex, warningIndex } ->
            benchIndex * 10 + warningIndex

        HoverBefore benchIndex ->
            -benchIndex - 1

        HoverAfter benchIndex ->
            -benchIndex - 10 - 1


fromHoverId : Int -> HoverBeaconLocation
fromHoverId id =
    if id >= 0 then
        LifeBlockWarning
            { benchIndex = tensPlace id
            , warningIndex = modBy 10 id
            }

    else if id < 0 && id >= -10 then
        HoverBefore <| -id - 1

    else
        HoverAfter <| -id - 10 - 1


tensPlace : Int -> Int
tensPlace n =
    if n >= 0 then
        (n - modBy 10 n) // 10

    else
        -1 * tensPlace -n



-- DECODE


decoders : DragState.IdDecoders DragBeaconId DropBeaconId HoverBeaconId
decoders =
    { toDragId = DragBeaconId
    , toDropId = DropBeaconId
    , toHoverId = HoverBeaconId
    }



-- ATTRIBUTE


beaconAttribute : String -> Int -> Element.Attribute msg
beaconAttribute attr id =
    id
        |> Encode.int
        |> Encode.encode 0
        |> Html.Attributes.attribute attr
        |> Element.htmlAttribute
