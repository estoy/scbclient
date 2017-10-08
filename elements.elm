module Elements exposing (titleElement)

import Styles exposing (..)

-- External ---------------------


import Element exposing (el, paragraph, text, Element)

titleElement : String -> Element Styles variation msg
titleElement title =
    el TableTitle [] <| paragraph None [] <| [text title]