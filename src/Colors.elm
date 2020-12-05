module Colors exposing
    ( black
    , blue
    , darkened
    , darkenedHtml
    , disabledRed
    , failureGlow
    , faint
    , faintHtml
    , red
    , successGlow
    , transparent
    , white
    )

import Element exposing (Attribute, Color, fromRgb255, rgb255, rgba255)
import Element.Border as Border
import Html
import Html.Attributes


type alias Rgb255 =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


red : Color
red =
    fromRgb255 redVals


blue : Color
blue =
    rgb255 45 49 166


disabledRed : Color
disabledRed =
    fromRgb255 { redVals | alpha = 0.5 }


redVals : Rgb255
redVals =
    { red = 196
    , green = 32
    , blue = 14
    , alpha = 1.0
    }


darkened : Color
darkened =
    rgba255 0 0 0 0.8


darkenedHtml : Html.Attribute msg
darkenedHtml =
    Html.Attributes.style "color" "rgba(0,0,0,0.8)"


faint : Color
faint =
    rgba255 0 0 0 0.1


faintHtml : Html.Attribute msg
faintHtml =
    Html.Attributes.style "color" "rgba(0,0,0,0.1)"


transparent : Color
transparent =
    rgba255 0 0 0 0.0


successGlow : Attribute msg
successGlow =
    Border.glow blue 0.75


failureGlow : Attribute msg
failureGlow =
    Border.glow red 0.75
