module Api exposing (healthCheck)

import Http


healthCheck : (Result Http.Error () -> msg) -> Cmd msg
healthCheck toMsg =
    Http.get
        { url = "api/health-check"
        , expect = Http.expectWhatever toMsg
        }
