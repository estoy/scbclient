module Attributes exposing (..)

import Element exposing (Attribute)
import Element.Attributes exposing (verticalCenter, spacing, padding, paddingRight, paddingXY, justify, yScrollbar, scrollbars, maxHeight, px)


columnAttributes : List (Attribute variation msg)
columnAttributes =
    [ spacing 20, padding 20 ]


listAttributes : List (Attribute variation msg)
listAttributes =
    [ spacing 5, paddingXY 10 0 ]
