module Common exposing
    ( clamp
    , dagger
    , edges
    , onEnter
    , pairDecoder
    , sectionSign
    , userSelectNone
    )

import Element
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)


dagger : Char
dagger =
    Char.fromCode 8224


sectionSign : Char
sectionSign =
    Char.fromCode 167


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


clamp : comparable -> ( comparable, comparable ) -> comparable
clamp val ( minimum, maximum ) =
    min (max val minimum) maximum


pairDecoder : Decoder a -> Decoder b -> Decoder ( a, b )
pairDecoder decodeA decodeB =
    Decode.map2 Tuple.pair
        (Decode.index 0 decodeA)
        (Decode.index 1 decodeB)


onEnter : msg -> Element.Attribute msg
onEnter =
    onKeyUp enterKeyDecoder


onKeyUp : (msg -> String -> Decoder msg) -> msg -> Element.Attribute msg
onKeyUp decode msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen (decode msg)
            )
        )


enterKeyDecoder : msg -> String -> Decoder msg
enterKeyDecoder msg key =
    if key == "Enter" then
        Decode.succeed msg

    else
        Decode.fail "Not the enter key"


userSelectNone : List (Element.Attribute msg)
userSelectNone =
    List.map (\key -> Element.htmlAttribute <| Html.Attributes.style key "none")
        [ "-webkit-touch-callout"
        , "-webkit-user-select"
        , "-khtml-user-select"
        , "-moz-user-select"
        , "-ms-user-select"
        , "user-select"
        ]
