module Model.Worksheet.ShadedStats exposing (..)

import Model.Worksheet.Shade exposing (Shade)
import Model.Worksheet.Stat as Stat exposing (Stat)


type alias ShadedStat =
    { value : Int
    , shade : Shade
    }


updateStat : ShadedStat -> Int -> ShadedStat
updateStat stat value =
    { stat | value = value }


updateShade : ShadedStat -> Shade -> ShadedStat
updateShade stat shade =
    { stat | shade = shade }


type alias ShadedStats =
    { will : ShadedStat
    , perception : ShadedStat
    , power : ShadedStat
    , forte : ShadedStat
    , agility : ShadedStat
    , speed : ShadedStat
    }


get : Stat -> ShadedStats -> ShadedStat
get stat shadedStats =
    case stat of
        Stat.Will ->
            shadedStats.will

        Stat.Perception ->
            shadedStats.perception

        Stat.Power ->
            shadedStats.power

        Stat.Forte ->
            shadedStats.forte

        Stat.Agility ->
            shadedStats.agility

        Stat.Speed ->
            shadedStats.speed


updateStatVal : Stat -> Int -> ShadedStats -> ShadedStats
updateStatVal stat value stats =
    let
        { shade } =
            get stat stats

        newStat : ShadedStat
        newStat =
            { value = value, shade = shade }
    in
    case stat of
        Stat.Will ->
            { stats | will = newStat }

        Stat.Perception ->
            { stats | perception = newStat }

        Stat.Power ->
            { stats | power = newStat }

        Stat.Forte ->
            { stats | forte = newStat }

        Stat.Agility ->
            { stats | agility = newStat }

        Stat.Speed ->
            { stats | speed = newStat }


updateStatShade : Stat -> Shade -> ShadedStats -> ShadedStats
updateStatShade stat shade shadedStats =
    case stat of
        Stat.Will ->
            { shadedStats | will = updateShade shadedStats.will shade }

        Stat.Perception ->
            { shadedStats | perception = updateShade shadedStats.perception shade }

        Stat.Power ->
            { shadedStats | power = updateShade shadedStats.power shade }

        Stat.Forte ->
            { shadedStats | forte = updateShade shadedStats.forte shade }

        Stat.Agility ->
            { shadedStats | agility = updateShade shadedStats.agility shade }

        Stat.Speed ->
            { shadedStats | speed = updateShade shadedStats.speed shade }
