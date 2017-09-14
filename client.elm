module Client exposing (..)

import Types exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (..)
import Color
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font


initialModel : Model
initialModel =
    { selectedLanguage =
        sites
            |> List.map .language
            |> List.head
            |> Maybe.withDefault ""
    }


view : Model -> Html msg
view model =
    viewport stylesheet <|
        column Main
            [ spacing 20, padding 20 ]
            (List.map (elementFromSite model.selectedLanguage) sites)


update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )


main : Program Never Model msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }


elementFromSite : String -> Site -> Element Styles variation msg
elementFromSite language site =
    let
        style = if site.language == language then Selected else None
    in
        el style [] (text site.language)


sites : List Site
sites =
    [ { language = "Svenska", url = "http://api.scb.se/OV0104/v1/doris/sv/ssd" }
    , { language = "English", url = " http://api.scb.se/OV0104/v1/doris/en/ssd" }
    ]


type Styles
    = None
    | Main
    | Selected


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Main
            [ Color.text Color.darkCharcoal
            , Color.background Color.white
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 16
            , Font.lineHeight 1.3
            ]
        , style Selected
            [ Color.text Color.white
            , Color.background Color.charcoal
            ]
        ]
