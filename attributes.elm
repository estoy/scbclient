module Attributes exposing (columnAttributes, listAttributes)

-- External ------

import Element exposing (Attribute)
import Element.Attributes exposing (spacing, padding, paddingXY)


columnAttributes : List (Attribute variation msg)
columnAttributes =
    [ spacing 20, padding 20 ]


listAttributes : List (Attribute variation msg)
listAttributes =
    [ spacing 5, paddingXY 10 0 ]
