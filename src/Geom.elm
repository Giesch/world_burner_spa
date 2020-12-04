module Geom exposing (..)

import Json.Decode as Decode


type alias Point =
    { x : Float
    , y : Float
    }


type alias Box =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


bounds : Box -> Point -> Bool
bounds box cursor =
    (cursor.x > box.x)
        && (cursor.y > box.y)
        && (cursor.x < box.x + box.width)
        && (cursor.y < box.y + box.height)


center : Box -> Point
center { x, y, width, height } =
    { x = x + (width / 2)
    , y = y + (height / 2)
    }


distance : Point -> Point -> Float
distance a b =
    let
        dx =
            a.x - b.x

        dy =
            a.y - b.y
    in
    sqrt <| (dx ^ 2) + (dy ^ 2)



-- DECODE


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.map2 Point
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


boxDecoder : Decode.Decoder Box
boxDecoder =
    Decode.map4 Box
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)
