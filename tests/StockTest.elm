module StockTest exposing (..)

import ExampleJson
import Expect
import Json.Decode as Decode
import Model.Lifepath exposing (Lifepath)
import Model.Stock as Stock exposing (Stock)
import Test exposing (..)


stockDecoder : Test
stockDecoder =
    describe "Stock decoder"
        [ test "can decode the dwarves" <|
            \_ ->
                ExampleJson.dwarves
                    |> Decode.decodeString Stock.decoder
                    |> Expect.ok
        ]
