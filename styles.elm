module Styles exposing (..)

-- External ------

import Color
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font exposing (font)
import Style.Border as Border


type Styles
    = None
    | Main
    | Disabled
    | Selected
    | Deselected
    | Site
    | Table
    | TableTitle
    | VariableName
    | VariableData
    | DimBox
    | DataBox
    | HeaderBox
    | DataGrid
    | PlotFuchsia
    | PlotRed
    | PlotBlue
    | PlotGreen
    | PlotMaroon
    | PlotPurple
    | PlotAqua
    | PlotOlive
    | PlotBlack


tableBackground =
    Color.rgba 231 214 166 1.0


dataBackground =
    Color.rgba 239 227 195 1.0


baseStyle : List (Style.Property class variation)
baseStyle =
    [ Font.typeface [ font "helvetica", font "arial", font "sans-serif" ]
    , Font.size 16
    , Font.lineHeight 1.3
    ]


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Main
            ([ Color.text Color.darkCharcoal
             , Color.background Color.lightGrey
             ]
                ++ baseStyle
            )
            , style Disabled
            ([ Color.text Color.lightCharcoal
             , Color.background Color.lightGray
             ]
                ++ baseStyle
            )
        , style Selected
            [ Color.text Color.white
            , Color.background Color.charcoal
            , cursor "pointer"
            ]
        , style Deselected
            [ cursor "pointer"
            ]
        , style Site
            [ Color.background (Color.rgba 186 196 238 1.0) ]
        , style Table <|
            [ Color.text Color.darkCharcoal
             , Color.background tableBackground
             ]
                ++ baseStyle
            
        , style TableTitle
            [ Font.size 24, Font.bold ]
        , style VariableName
            [ Font.bold ]
        , style VariableData
            [ Color.background dataBackground
            , cursor "pointer"
            ]
        , style DimBox
            [ Border.all 1.0
            , Font.size 12
            , Font.lineHeight 1.2
            ]
        , style HeaderBox
            [ Border.all 1.0
            , Font.size 12
            , Font.bold
            , Font.lineHeight 1.2
            ]
        , style DataBox
            [ Border.all 1.0
            , Font.size 12
            , Font.lineHeight 1.2
            , Color.background (Color.rgba 186 196 238 1.0)
            ]
        , style DataGrid
            [ Color.background dataBackground ]
        , style PlotFuchsia <|
            [ Color.text <| Color.rgba 255 0 255 1.0] ++ baseStyle
         , style PlotRed <|
            [ Color.text <| Color.rgba 255 0 0 1.0] ++ baseStyle
        , style PlotBlue <|
            [ Color.text <| Color.rgba 0 0 255 1.0] ++ baseStyle
        , style PlotGreen <|
            [ Color.text <| Color.rgba 0 128 0 1.0] ++ baseStyle
        , style PlotMaroon <|
            [ Color.text <| Color.rgba 128 0 0 1.0] ++ baseStyle
        , style PlotPurple <|
            [ Color.text <| Color.rgba 128 0 128 1.0] ++ baseStyle
        , style PlotAqua <|
            [ Color.text <| Color.rgba 0 255 255 1.0] ++ baseStyle
        , style PlotOlive <|
            [ Color.text <| Color.rgba 128 128 0 1.0] ++ baseStyle
        , style PlotBlack <|
            [ Color.text <| Color.rgba 0 0 0 1.0] ++ baseStyle
       ]


