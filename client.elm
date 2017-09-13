module Client exposing (..)

import Types exposing (..)
import Html exposing (..)


initialModel : Model
initialModel =
    { message = "Hello, World!" }


view : Model -> Html msg
view model =
    div [] [ text model.message ]


update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none)


main : Program Never Model msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
