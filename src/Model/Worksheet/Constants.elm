module Model.Worksheet.Constants exposing (..)


type alias AgeTableRow =
    { minAge : Int
    , maxAge : Int
    , physical : Int
    , mental : Int
    }


dwarfAgeTable : List AgeTableRow
dwarfAgeTable =
    [ { minAge = 1
      , maxAge = 20
      , mental = 6
      , physical = 13
      }
    , { minAge = 21
      , maxAge = 30
      , mental = 7
      , physical = 13
      }
    , { minAge = 31
      , maxAge = 50
      , mental = 7
      , physical = 14
      }
    , { minAge = 51
      , maxAge = 76
      , mental = 8
      , physical = 15
      }
    , { minAge = 77
      , maxAge = 111
      , mental = 8
      , physical = 16
      }
    , { minAge = 112
      , maxAge = 151
      , mental = 9
      , physical = 16
      }
    , { minAge = 152
      , maxAge = 199
      , mental = 9
      , physical = 17
      }
    , { minAge = 200
      , maxAge = 245
      , mental = 10
      , physical = 18
      }
    , { minAge = 246
      , maxAge = 300
      , mental = 11
      , physical = 17
      }
    , { minAge = 301
      , maxAge = 345
      , mental = 11
      , physical = 16
      }
    , { minAge = 346
      , maxAge = 396
      , mental = 12
      , physical = 15
      }
    , { minAge = 397
      , maxAge = 445
      , mental = 11
      , physical = 14
      }
    , { minAge = 446
      , maxAge = 525
      , mental = 11
      , physical = 13
      }
    , { minAge = 526
      , maxAge = 600
      , mental = 10
      , physical = 12
      }
    ]
