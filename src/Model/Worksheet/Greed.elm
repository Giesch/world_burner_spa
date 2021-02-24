module Model.Worksheet.Greed exposing
    ( AttributeQuestion
    , CalculatedModifier
    , Modifier
    , ModifierCalculation(..)
    , RelationshipKind(..)
    , modifiers
    )

import Model.Worksheet.Stat as Stat exposing (Stat)


type Modifier
    = Question AttributeQuestion
    | Calculated CalculatedModifier


type alias AttributeQuestion =
    { text : String
    , modifier : Int
    }


type alias CalculatedModifier =
    { modifier : ModifierCalculation
    , text : String
    }


type ModifierCalculation
    = StatLessThan ( Stat, Int )
    | StatGreaterThan ( Stat, Int )
    | PerResPoints { res : Int, mod : Int }
    | PerLifepath (List String) -- lifepath names (should be ids)
    | AgeLessThan { age : Int, mod : Int }
    | AgeGreaterThan { age : Int, mod : Int }
    | PerRelationship (List ( List RelationshipKind, Int ))
    | PerTrait { traitName : String, mod : Int } -- should be id


type RelationshipKind
    = Romantic
    | Hateful
    | ImmediateFamily


modifiers : List Modifier
modifiers =
    [ Calculated
        { modifier = StatLessThan ( Stat.Will, 5 )
        , text = "+1 Greed if Will exponent is 4 or lower."
        }
    , Calculated
        { modifier = PerResPoints { res = 60, mod = 1 }
        , text = "+1 Greed for every 60 resource points"
        }
    , Calculated
        { modifier = PerLifepath greedyLifepaths
        , text = "+1 Greed for each of the following lifepaths: Trader, Mask Bearer, Master of Arches, Master of Forges, Master Engraver, Treasurer, Quartermaster, Seneschal, Prince"
        }
    , Question
        { text = "Has the Dwarf coveted something owned by another? If so, +1 Greed."
        , modifier = 1
        }
    , Question
        { text = "Has the Dwarf ever stolen something they coveted? If so, +1 Greed."
        , modifier = 1
        }
    , Question
        { text = "Has the Dwarf ever had their prized treasure stolen from them? If so, +1 Greed."
        , modifier = 1
        }
    , Question
        { text = "Has the Dwarf ever been in the presence of the master craftsmanship of the Dwarven Fathers? If so, +1 Greed."
        , modifier = 1
        }
    , Question
        { text = "Has the Dwarf witessed an outsider (Elf, Human, Orc, Roden, etc.) in possession of a work of Dwarven Art? If so, +1 Greed."
        , modifier = 1
        }
    , Calculated
        { modifier = AgeGreaterThan { age = 200, mod = 1 }
        , text = "Is the Dwarf over 200 years old? If so, +1 Greed."
        }
    , Calculated
        { modifier = AgeGreaterThan { age = 400, mod = 2 }
        , text = "Is the Dwarf over 400 years old? If so, +2 Greed."
        }
    , Calculated
        { modifier =
            PerRelationship
                [ ( [ Romantic ], -1 )
                , ( [ Hateful ], 1 )
                , ( [ Hateful, ImmediateFamily ], 2 )
                ]
        , text = "Each romantic relationship is -1 Greed. Each hateful relationship is +1 Greed. A hateful immediate family member is +2 Greed."
        }
    , Calculated
        { modifier = PerTrait { traitName = "virtuous", mod = -1 }
        , text = "If you have the Virtuous trait, -1 Greed."
        }
    ]


greedyLifepaths : List String
greedyLifepaths =
    [ "trader"
    , "mask bearer"
    , "master of arches"
    , "master of forges"
    , "master engraver"
    , "treasurer"
    , "quartermaster"
    , "seneschal"
    , "prince"
    ]
