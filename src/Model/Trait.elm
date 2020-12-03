module Model.Trait exposing
    ( Trait
    , TraitType
    , cost
    , decoder
    , entry
    , name
    , taip
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)


type Trait
    = TraitEntry TraitEntryProps
    | CharTrait String


type alias TraitEntryProps =
    { name : String
    , traitId : Int
    , page : Int
    , cost : Maybe Int
    , taip : TraitType
    }


type TraitType
    = Char
    | CallOn
    | Die


entry : Trait -> Maybe { traitId : Int, page : Int }
entry trait =
    case trait of
        CharTrait _ ->
            Nothing

        TraitEntry props ->
            Just { traitId = props.traitId, page = props.page }


name : Trait -> String
name trait =
    case trait of
        CharTrait char ->
            char

        TraitEntry props ->
            props.name


cost : Trait -> Maybe Int
cost trait =
    case trait of
        CharTrait _ ->
            Just 1

        TraitEntry props ->
            props.cost


taip : Trait -> TraitType
taip trait =
    case trait of
        CharTrait _ ->
            Char

        TraitEntry props ->
            props.taip



-- DECODING


decoder : Decoder Trait
decoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen whichTraitType


whichTraitType : String -> Decoder Trait
whichTraitType string =
    case String.toLower string of
        "traitentry" ->
            Decode.field "value" traitEntryDecoder

        "chartrait" ->
            Decode.field "value" charTraitDecoder

        _ ->
            Decode.fail <| "Invalid trait type: " ++ string


traitEntryDecoder : Decoder Trait
traitEntryDecoder =
    Decode.map TraitEntry <|
        (Decode.succeed TraitEntryProps
            |> required "name" Decode.string
            |> required "trait_id" Decode.int
            |> required "page" Decode.int
            |> optional "cost" (Decode.map Just Decode.int) Nothing
            |> required "taip" traitTypeDecoder
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
    Decode.map CharTrait <| Decode.field "name" Decode.string
