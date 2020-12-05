module Spa.Document exposing
    ( Document
    , map
    , toBrowserDocument
    )

import Browser
import Element exposing (..)


type alias Document msg =
    { title : String
    , body : List (Element msg)
    , modal : Maybe (Element msg)
    }


map : (msg1 -> msg2) -> Document msg1 -> Document msg2
map fn doc =
    { title = doc.title
    , body = List.map (Element.map fn) doc.body
    , modal = Maybe.map (Element.map fn) doc.modal
    }


toBrowserDocument : Document msg -> Browser.Document msg
toBrowserDocument doc =
    let
        layoutAttributes =
            case doc.modal of
                Just modalView ->
                    [ width fill, height fill, inFront modalView ]

                Nothing ->
                    [ width fill, height fill ]
    in
    { title = doc.title
    , body =
        [ Element.layout layoutAttributes
            (column [ width fill, height fill ] doc.body)
        ]
    }
