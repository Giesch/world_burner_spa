module Api exposing
    ( healthCheck
    , stocks
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Model.Stock as Stock exposing (Stock)


stocks : (Result Http.Error (List Stock) -> msg) -> Cmd msg
stocks toMsg =
    Http.get
        { url = "/api/stocks"
        , expect = Http.expectJson toMsg stocksResponseDecoder
        }


stocksResponseDecoder : Decoder (List Stock)
stocksResponseDecoder =
    Decode.field "stocks" (Decode.list Stock.decoder)


healthCheck : (Result Http.Error () -> msg) -> Cmd msg
healthCheck toMsg =
    Http.get
        { url = "api/health-check"
        , expect = Http.expectWhatever toMsg
        }
