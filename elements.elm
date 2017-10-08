module Elements exposing (titleElement,  buttonElement)

import Styles exposing (..)
import Types exposing (..)
import Attributes exposing (buttonHeight)

-- External ---------------------

import Element exposing (el, paragraph, text, Element, button)
import Element.Events exposing (onClick)

titleElement : String -> Element Styles variation msg
titleElement title =
    el TableTitle [] <| paragraph None [] <| [text title]

buttonElement : String -> Msg -> Bool -> Element Styles variation Msg
buttonElement title msg enabled =
    let
        ( buttonStyle, buttonAttributes ) =
            if enabled then
                ( Main, [ onClick msg ] )
            else
                ( Disabled, [] )
    in
        button buttonStyle (buttonHeight :: buttonAttributes)  <| text title