module Model.Lifepath.Requirement exposing
    ( LifepathPredicate
    , Predicate(..)
    , Requirement
    , SettingPredicate
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import List.NonEmpty as NonEmpty exposing (NonEmpty)


type alias Requirement =
    { predicate : Predicate
    , description : String
    }


type Predicate
    = SpecificLifepath LifepathPredicate
    | PreviousLifepaths Int
    | Setting SettingPredicate
    | Any (NonEmpty Predicate)
    | All (NonEmpty Predicate)


type alias LifepathPredicate =
    { lifepathId : Int
    , count : Int
    }


type alias SettingPredicate =
    { settingId : Int
    , count : Int
    }



-- DECODE


decoder : Decoder Requirement
decoder =
    Decode.succeed Requirement
        |> required "predicate" predicateDecoder
        |> required "description" Decode.string


predicateDecoder : Decoder Predicate
predicateDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen predicateDecoderFromType


predicateDecoderFromType : String -> Decoder Predicate
predicateDecoderFromType string =
    let
        value : Decoder a -> Decoder a
        value =
            Decode.field "value"
    in
    case string of
        "lifepath" ->
            value lifepathPredicateDecoder

        "previousLifepaths" ->
            value previousDecoder

        "setting" ->
            value settingPredicateDecoder

        "any" ->
            value <| Decode.lazy (\_ -> anyDecoder)

        "all" ->
            value <| Decode.lazy (\_ -> allDecoder)

        _ ->
            Decode.fail ("Invalid predicate type: " ++ string)


lifepathPredicateDecoder : Decoder Predicate
lifepathPredicateDecoder =
    Decode.map SpecificLifepath <|
        (Decode.succeed LifepathPredicate
            |> required "lifepathId" Decode.int
            |> required "count" Decode.int
        )


previousDecoder : Decoder Predicate
previousDecoder =
    Decode.map PreviousLifepaths Decode.int


settingPredicateDecoder : Decoder Predicate
settingPredicateDecoder =
    Decode.map Setting <|
        (Decode.succeed SettingPredicate
            |> required "settingId" Decode.int
            |> required "count" Decode.int
        )


anyDecoder : Decoder Predicate
anyDecoder =
    Decode.map Any <| NonEmpty.decodeList predicateDecoder


allDecoder : Decoder Predicate
allDecoder =
    Decode.map All <| NonEmpty.decodeList predicateDecoder
