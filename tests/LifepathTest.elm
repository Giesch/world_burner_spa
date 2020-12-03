module LifepathTest exposing (..)

import Api
import ExampleLifepathJson exposing (lifepathsResponse)
import Expect
import Json.Decode as Decode
import Model.Lifepath exposing (Lifepath)
import Test exposing (..)


lifepathsDecoder : Test
lifepathsDecoder =
    describe "Lifepaths decoder"
        [ test "can decode the dwarves" <|
            \_ ->
                lifepathsResponse
                    |> Decode.decodeString Api.lifepathsDecoder
                    |> Expect.ok
        ]
