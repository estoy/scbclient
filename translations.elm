module Translations exposing (translate)

import Dict exposing (..)

translate : String -> String -> String
translate key language =
    let
        lang =
            String.toLower language
    in
        translations
            |> Dict.get lang
            |> Maybe.withDefault emptyTranslation
            |> Dict.get key
            |> Maybe.withDefault "*missing*"
    
english : Dict String String
english =
    Dict.fromList
        [ ("plot", "Plot") 
        , ("sort", "Sort") 
        , ("selectall", "Select all")
        , ("submit", "Submit")
        ]

swedish : Dict String String
swedish =
    Dict.fromList
        [ ("plot", "Plotta") 
        , ("sort", "Sortera") 
        , ("selectall", "Välj alla")
        , ("submit", "Skicka")
        ]

emptyTranslation : Dict String String
emptyTranslation =
    Dict.fromList []
   

translations : Dict String (Dict String String)
translations =
    Dict.fromList
        [ ("english", english)
        , ("svenska", swedish)
        ]