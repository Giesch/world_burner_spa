module Model.Lifepath.StatMod exposing
    ( Bonus
    , StatMod
    , addBonus
    , bonus
    , decoder
    , noBonus
    , none
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Model.NonZero as NonZero exposing (NonZero)


type StatMod
    = Physical NonZero
    | Mental NonZero
    | Either NonZero
    | Both NonZero
    | None


none : StatMod
none =
    None


type alias Bonus =
    { physical : Int
    , mental : Int
    , either : Int
    }


noBonus : Bonus
noBonus =
    { physical = 0, mental = 0, either = 0 }


addBonus : Bonus -> Bonus -> Bonus
addBonus left right =
    { physical = left.physical + right.physical
    , mental = left.mental + right.mental
    , either = left.either + right.either
    }


bonus : StatMod -> Bonus
bonus statMod =
    case statMod of
        Physical nz ->
            { noBonus | physical = NonZero.toInt nz }

        Mental nz ->
            { noBonus | mental = NonZero.toInt nz }

        Either nz ->
            { noBonus | either = NonZero.toInt nz }

        Both nz ->
            { noBonus | physical = NonZero.toInt nz, mental = NonZero.toInt nz }

        None ->
            noBonus


toString : StatMod -> String
toString statMod =
    case statMod of
        None ->
            "stat: --"

        Physical val ->
            "stat: " ++ viewVal val ++ "P"

        Mental val ->
            "stat: " ++ viewVal val ++ "M"

        Either val ->
            "stat: " ++ viewVal val ++ "M/P"

        Both val ->
            "stat: " ++ viewVal val ++ "M,P"


viewVal : NonZero -> String
viewVal val =
    let
        v =
            NonZero.toInt val
    in
    if v > 0 then
        "+" ++ String.fromInt v

    else
        "-" ++ String.fromInt v


decoder : Decoder StatMod
decoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen statModKindsDecoder


statModKindsDecoder : String -> Decoder StatMod
statModKindsDecoder kind =
    case kind of
        "physical" ->
            modVariantDecoder Physical

        "mental" ->
            modVariantDecoder Mental

        "either" ->
            modVariantDecoder Either

        "both" ->
            modVariantDecoder Both

        "none" ->
            Decode.succeed None

        _ ->
            Decode.fail <| "Invalid stat mod kind: " ++ kind


modVariantDecoder : (NonZero -> StatMod) -> Decoder StatMod
modVariantDecoder variant =
    Decode.map variant (Decode.field "value" NonZero.decode)
