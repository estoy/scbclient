module Client exposing (..)

import Types exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)


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
        column None
            [ spacing 20, padding 20 ]
            (List.map (\site -> el None [] (text site.language)) sites)


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


sites : List Site
sites =
    [ { language = "Svenska", url = "http://api.scb.se/OV0104/v1/doris/sv/ssd" }
    , { language = "English", url = " http://api.scb.se/OV0104/v1/doris/en/ssd" }
    ]


type Styles
    = None


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None [] ]
