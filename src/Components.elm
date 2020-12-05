module Components exposing (tooltip)

import Element exposing (..)
import Html.Attributes


tooltip : (Element msg -> Attribute msg) -> Element Never -> Attribute msg
tooltip position tooltipElem =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (position << Element.map never) <|
                el [ htmlAttribute (Html.Attributes.style "pointerEvents" "none") ]
                    tooltipElem
            ]
            none
