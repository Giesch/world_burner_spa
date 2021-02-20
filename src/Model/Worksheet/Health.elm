module Model.Worksheet.Health exposing (Answers, compute)

import Model.Worksheet.Shade as Shade exposing (Shade)
import Model.Worksheet.ShadedStats as ShadedStats exposing (ShadedStats)


type alias Answers =
    { squalor : Bool
    , frail : Bool
    , wounded : Bool
    , enslaved : Bool
    , immortal : Bool
    , athletic : Bool
    , soundOfMusic : Bool
    }


compute : ShadedStats -> Answers -> ( Shade, Int )
compute stats answers =
    Tuple.mapSecond
        (\val -> val + modifier answers)
        (base stats)


base : ShadedStats -> ( Shade, Int )
base stats =
    let
        val : Int
        val =
            floor <| toFloat (stats.will.value + stats.forte.value) / 2
    in
    case ( stats.will.shade, stats.forte.shade ) of
        ( Shade.Gray, Shade.Gray ) ->
            ( Shade.Gray, val )

        ( Shade.Black, Shade.Black ) ->
            ( Shade.Black, val )

        _ ->
            ( Shade.Black, val + 2 )


modifier : Answers -> Int
modifier answers =
    [ ( answers.squalor, -1 )
    , ( answers.frail, -1 )
    , ( answers.wounded, -1 )
    , ( answers.enslaved, -1 )
    , ( answers.immortal, 1 )
    , ( answers.athletic, 1 )
    , ( answers.soundOfMusic, 1 )
    ]
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> List.sum
