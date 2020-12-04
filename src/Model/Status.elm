module Model.Status exposing
    ( Status(..)
    , map
    )


type Status a
    = Loading
    | Loaded a
    | Failed


map : (a -> b) -> Status a -> Status b
map fn status =
    case status of
        Loading ->
            Loading

        Failed ->
            Failed

        Loaded a ->
            Loaded (fn a)
