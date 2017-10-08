module Elements exposing (buttonElement, titleRow)

import Styles exposing (..)
import Types exposing (..)
import Attributes exposing (buttonHeight)


-- External ---------------------

import Element exposing (el, paragraph, text, Element, button, row)
import Element.Attributes exposing (spread, height, fill, width, percent, spacing)
import Element.Events exposing (onClick)


titleElement : String -> Element Styles variation msg
titleElement title =
    el TableTitle [] <| paragraph None [] <| [ text title ]


buttonElement : String -> Msg -> Bool -> Button variation
buttonElement title msg enabled =
    let
        ( buttonStyle, buttonAttributes ) =
            if enabled then
                ( Main, [ onClick msg ] )
            else
                ( Disabled, [] )
    in
        button buttonStyle (buttonHeight :: buttonAttributes) <| text title


titleRow : String -> List (Button variation) -> Element Styles variation Msg
titleRow title buttons =
    row None
        [ spread, spacing 10 ]
        [ titleElement title
        , (row None
            [ spacing 5 ]
            buttons
          )
        ]
