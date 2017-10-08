module Translations exposing (translate)

import Types exposing (..)
import Dict exposing (..)


translate : TranslationKey -> String -> String
translate key language =
    let
        lang =
            String.toLower language
    in
        translations
            |> Dict.get lang
            |> Maybe.withDefault emptyTranslation
            |> lookup key
            |> Maybe.withDefault "*missing*"


english : List ( TranslationKey, String )
english =
    [ ( PlotKey, "Plot" )
    , ( SortKey, "Sort" )
    , ( SelectAllKey, "Select all" )
    , ( SubmitKey, "Submit" )
    , ( ClearSelectionsKey, "Clear all selections" )
    ]


swedish : List ( TranslationKey, String )
swedish =
    [ ( PlotKey, "Plotta" )
    , ( SortKey, "Sortera" )
    , ( SelectAllKey, "Välj alla" )
    , ( SubmitKey, "Skicka" )
    , ( ClearSelectionsKey, "Rensa alla val" )
    ]


emptyTranslation : List ( TranslationKey, String )
emptyTranslation =
    []


translations : Dict String (List ( TranslationKey, String ))
translations =
    Dict.fromList
        [ ( "english", english )
        , ( "svenska", swedish )
        ]

lookup : TranslationKey -> List ( TranslationKey, String ) -> Maybe String
lookup key table =
    table
        |> List.filter (\(k, v) -> k == key)
        |> List.map (\(k, v) -> v)
        |> List.head