module Model.Lifepath.LifepathIndex exposing
    ( LifepathIndex
    , lifepaths
    , new
    , search
    )

{-| A wrapper module around ElmTextSearch.
Searches lifepaths by name, setting, skills, and traits.
-}

import Dict exposing (Dict)
import ElmTextSearch
import Model.Lifepath exposing (Lifepath)
import Model.Lifepath.Trait as Trait


type LifepathIndex
    = LifepathIndex Internal


type alias Internal =
    { index : ElmTextSearch.Index Lifepath
    , lifepathsById : Dict String Lifepath
    }


{-| Create a new search index.
This will fail if:

1.  The given lifepath list is empty,
2.  All search fields are empty/all stop words,
3.  There are duplicate lifepath ids.

-}
new : List Lifepath -> Maybe LifepathIndex
new paths =
    case ElmTextSearch.addDocs paths <| ElmTextSearch.new config of
        ( index, [] ) ->
            let
                lifepathsById =
                    List.foldl
                        (\lp dict -> Dict.insert (String.fromInt lp.id) lp dict)
                        Dict.empty
                        paths
            in
            Just <| LifepathIndex { index = index, lifepathsById = lifepathsById }

        _ ->
            -- NOTE The possible errors here come from empty or duplicate ids, or a document that is all stop words
            -- See 'add' & 'addDocs': https://package.elm-lang.org/packages/rluiten/elm-text-search/latest/ElmTextSearch
            Nothing


config : ElmTextSearch.SimpleConfig Lifepath
config =
    { ref = .id >> String.fromInt
    , fields =
        [ ( .name, 2.0 )
        , ( .settingName, 0.5 )
        ]
    , listFields =
        [ ( .skills >> List.map .displayName, 1.0 )
        , ( .traits >> List.map Trait.name, 1.0 )
        ]
    }


{-| Search the index for a given term, and update the index for future searches.
Will return no hits if the term is empty or all stop words.
-}
search : String -> LifepathIndex -> ( LifepathIndex, List Lifepath )
search term (LifepathIndex { index, lifepathsById }) =
    let
        lookupHit : ( String, Float ) -> Maybe Lifepath
        lookupHit ( id, _ ) =
            Dict.get id lifepathsById
    in
    case ElmTextSearch.search term index of
        Err _ ->
            -- NOTE the 3 possible errors all basically mean no hits for different reasons
            -- See 'search' https://package.elm-lang.org/packages/rluiten/elm-text-search/latest/ElmTextSearch
            ( LifepathIndex { index = index, lifepathsById = lifepathsById }, [] )

        Ok ( newIndex, hitList ) ->
            let
                hits =
                    List.filterMap lookupHit hitList
            in
            ( LifepathIndex { index = newIndex, lifepathsById = lifepathsById }, hits )


{-| Get the lifepaths from the index in id order.
-}
lifepaths : LifepathIndex -> List Lifepath
lifepaths (LifepathIndex { lifepathsById }) =
    Dict.values lifepathsById
