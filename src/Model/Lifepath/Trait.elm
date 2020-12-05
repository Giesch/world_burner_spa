module Model.Lifepath.Trait exposing
    ( Trait
    , TraitType
    , decode
    , kind
    , name
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)


type Trait
    = Entry TraitEntry
    | Entryless String


type alias TraitEntry =
    { name : String
    , kind : TraitType
    }


type TraitType
    = Char
    | CallOn
    | Die


name : Trait -> String
name trait =
    case trait of
        Entryless char ->
            char

        Entry props ->
            props.name


kind : Trait -> TraitType
kind trait =
    case trait of
        Entryless _ ->
            Char

        Entry props ->
            props.kind



-- DECODING


decode : Decoder Trait
decode =
    Decode.field "kind" Decode.string
        |> Decode.andThen traitKindDecoder


traitKindDecoder : String -> Decoder Trait
traitKindDecoder k =
    case k of
        "entry" ->
            Decode.field "value" traitEntryDecoder

        "entryless" ->
            Decode.field "value" charTraitDecoder

        _ ->
            Decode.fail <| "Invalid trait type: " ++ k


traitEntryDecoder : Decoder Trait
traitEntryDecoder =
    Decode.map Entry <|
        (Decode.succeed TraitEntry
            |> required "name" Decode.string
            |> required "kind" traitTypeDecoder
        )


traitTypeDecoder : Decoder TraitType
traitTypeDecoder =
    Decode.string |> Decode.andThen traitTypeFromString


traitTypeFromString : String -> Decoder TraitType
traitTypeFromString string =
    case String.toLower string of
        "char" ->
            Decode.succeed Char

        "callon" ->
            Decode.succeed CallOn

        "die" ->
            Decode.succeed Die

        _ ->
            Decode.fail <| "Invalid trait type: " ++ string


charTraitDecoder : Decoder Trait
charTraitDecoder =
    Decode.map Entryless Decode.string
