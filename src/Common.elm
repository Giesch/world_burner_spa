module Common exposing
    ( appendIntoNonEmpty
    , clamp
    , corners
    , edges
    , insertIntoNonEmpty
    , onEnter
    , pairDecoder
    , userSelectNone
    )

import Element
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import List.NonEmpty as NonEmpty exposing (NonEmpty)


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


corners : { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int }
corners =
    { topLeft = 0, topRight = 0, bottomLeft = 0, bottomRight = 0 }


clamp : ( comparable, comparable ) -> comparable -> comparable
clamp ( minimum, maximum ) val =
    min (max val minimum) maximum


appendIntoNonEmpty : List a -> a -> NonEmpty a
appendIntoNonEmpty list item =
    case list of
        [] ->
            NonEmpty.singleton item

        first :: rest ->
            ( first, rest ++ [ item ] )


insertIntoNonEmpty : List a -> a -> Int -> NonEmpty a
insertIntoNonEmpty list item index =
    let
        prefix : List a
        prefix =
            List.take index list

        suffix : List a
        suffix =
            List.drop index list
    in
    case prefix of
        [] ->
            ( item, suffix )

        first :: rest ->
            ( first, rest ++ item :: suffix )


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
        Decode.fail "Non-enter key"


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
