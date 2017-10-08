module Attributes exposing (columnAttributes, listAttributes, buttonHeight, titleAttributes)

-- External ------

import Element exposing (Attribute)
import Element.Attributes exposing (spacing, padding, paddingXY, height, px, spread)


columnAttributes : List (Attribute variation msg)
columnAttributes =
    [ spacing 20, padding 20 ]


listAttributes : List (Attribute variation msg)
listAttributes =
    [ spacing 5, paddingXY 10 0 ]


buttonHeight : Attribute variation msg
buttonHeight =
    height (px 30)


titleAttributes : List (Attribute variation msg)
titleAttributes =
    [ spread, spacing 10 ]
