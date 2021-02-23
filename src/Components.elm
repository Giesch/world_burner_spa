module Components exposing
    ( QuestionCheckboxOptions
    , deleteIcon
    , disabledCheckbox
    , dragHandle
    , faintButton
    , questionCheckbox
    , superScript
    , tooltip
    , warningIcon
    , warningsTooltip
    )

import Colors
import Common exposing (corners, edges)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon
import FontAwesome.Regular
import FontAwesome.Solid
import Html
import Html.Attributes


dragHandle : List (Attribute msg) -> Element msg
dragHandle dragStyles =
    Input.button
        ([ width <| px 20, height <| px 20 ]
            ++ Common.userSelectNone
            ++ dragStyles
        )
        { onPress = Nothing
        , label = text "⠶"
        }


warningsTooltip : List String -> Element msg
warningsTooltip warnings =
    el
        [ Background.color Colors.white
        , width fill
        , height fill
        , Font.color Colors.black
        , padding 5
        , Border.rounded 5
        , Font.size 16
        , Border.shadow
            { offset = ( 0, 3 )
            , blur = 6
            , size = 0
            , color = Colors.shadow
            }
        ]
    <|
        textColumn [ padding 5, spacing 5, width fill, height fill ] (List.map text warnings)


tooltip : (Element msg -> Attribute msg) -> Element Never -> Attribute msg
tooltip position tooltipElem =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (position << Element.map never) <|
                el [ htmlAttribute (Html.Attributes.style "pointerEvents" "none") ] tooltipElem
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


faintButton : String -> msg -> Element msg
faintButton label onPress =
    Input.button
        [ Background.color Colors.faint
        , Border.rounded 8
        , paddingXY 15 10
        , Font.size 18
        ]
        { onPress = Just onPress
        , label = text label
        }


type alias QuestionCheckboxOptions answers msg =
    { onChange : Bool -> answers
    , label : Element msg
    , checked : Bool
    , updateMsg : answers -> msg
    }


questionCheckbox : QuestionCheckboxOptions answers msg -> Element msg
questionCheckbox opts =
    Input.checkbox []
        { onChange = opts.onChange >> opts.updateMsg
        , icon = Input.defaultCheckbox
        , label =
            Input.labelLeft
                [ width fill, paddingEach { edges | right = 10 } ]
                opts.label
        , checked = opts.checked
        }


type alias DisabledCheckboxOptions msg =
    { label : Element msg
    , checked : Bool
    , noop : msg
    }


disabledCheckbox : DisabledCheckboxOptions msg -> Element msg
disabledCheckbox { label, checked, noop } =
    Input.checkbox
        [ Element.htmlAttribute <| Html.Attributes.style "cursor" "default"
        ]
        { onChange = \_ -> noop
        , icon = Input.defaultCheckbox
        , label =
            Input.labelLeft
                [ width fill
                , paddingEach { edges | right = 10 }
                , Font.color Colors.shadow
                ]
                label
        , checked = checked
        }
