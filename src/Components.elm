module Components exposing
    ( deleteIcon
    , superScript
    , tooltip
    , warningIcon
    )

import Colors
import Element exposing (..)
import FontAwesome.Icon as Icon
import FontAwesome.Regular
import FontAwesome.Solid
import Html
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


superScript : String -> Element msg
superScript script =
    Element.html <| Html.sup [] <| [ Html.text script ]


warningIcon : Element msg
warningIcon =
    appIcon FontAwesome.Solid.exclamationTriangle


deleteIcon : Element msg
deleteIcon =
    appIcon FontAwesome.Regular.trashAlt


appIcon : Icon.Icon -> Element msg
appIcon icon =
    html <| Icon.viewStyled [ Colors.darkenedHtml ] icon
