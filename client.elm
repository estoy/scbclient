module Client exposing (..)

import Types exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Style exposing (..)


initialModel : Model
initialModel =
    { message = "Hello, Elements World!" }


view : Model -> Html msg
view model =
    viewport (stylesheet []) (text model.message )


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
