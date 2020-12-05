module Api exposing
    ( LifepathsResponse
    , healthCheck
    , lifepaths
    , lifepathsDecoder
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Model.Lifepath as Lifepath exposing (Lifepath)


lifepaths : (Result Http.Error (List Lifepath) -> msg) -> Cmd msg
lifepaths toMsg =
    Http.get
        { url = "/api/lifepaths"
        , expect = Http.expectJson toMsg lifepathsDecoder
        }


lifepathsDecoder : Decoder (List Lifepath)
lifepathsDecoder =
    Decode.succeed LifepathsResponse
        |> required "lifepaths" (Decode.list Lifepath.decoder)
        |> Decode.map .lifepaths


type alias LifepathsResponse =
    { lifepaths : List Lifepath }


healthCheck : (Result Http.Error () -> msg) -> Cmd msg
healthCheck toMsg =
    Http.get
        { url = "api/health-check"
        , expect = Http.expectWhatever toMsg
        }
