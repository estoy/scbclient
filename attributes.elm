module Attributes exposing (columnAttributes, listAttributes, buttonHeight)

-- External ------

import Element exposing (Attribute)
import Element.Attributes exposing (spacing, padding, paddingXY, height, px)


columnAttributes : List (Attribute variation msg)
columnAttributes =
    [ spacing 20, padding 20 ]


listAttributes : List (Attribute variation msg)
listAttributes =
    [ spacing 5, paddingXY 10 0 ]


buttonHeight : Attribute variation msg
buttonHeight =
    height (px 30)
