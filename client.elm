module Client exposing (..)

import Types exposing (..)

import Json.Decode exposing (string, list, Decoder, at, map, lazy, oneOf, null)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, custom, optional)
import Http
import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (spacing, padding)
import Element.Events exposing (onClick)
import Color
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font


initialModel : Model
initialModel =
    { selectedSite = swedish
    , levels = []
    }


type Msg
    = SelectSite Site
    | LoadLevel Url
    | LevelLoaded (Result Http.Error (List Level))


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        row Main
            []
            [ column Main
                columnAttributes
                (List.map (elementFromSite model.selectedSite) sites)
            , column Main
                columnAttributes
                (List.map elementFromLevel model.levels)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectSite site ->
            update (LoadLevel site.url) { model | selectedSite = site }

        LoadLevel url ->
            ( model, loadLevelCmd url )

        LevelLoaded (Ok levels) ->
            ( { model | levels = levels }, Cmd.none )

        LevelLoaded (Err _) ->
            ( model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }


elementFromSite : Site -> Site -> Element Styles variation Msg
elementFromSite selected site =
    let
        style =
            if site == selected then
                Selected
            else
                None
    in
        el style [ onClick <| SelectSite site ] (text site.language)


elementFromLevel : Level -> Element Styles variation Msg
elementFromLevel level =
    el None [] (text level.text)


loadLevelCmd : Url -> Cmd Msg
loadLevelCmd url =
    list levelDecoder
        |> Http.get url
        |> Http.send (LevelLoaded)


levelDecoder : Decoder Level
levelDecoder =
    decode Level
        |> required "id" string
        |> required "type" string
        |> required "text" string


sites : List Site
sites =
    [ swedish
    , english
    ]


swedish : Site
swedish =
    { language = "Svenska", url = "http://api.scb.se/OV0104/v1/doris/sv/ssd" }


english : Site
english =
    { language = "English", url = " http://api.scb.se/OV0104/v1/doris/en/ssd" }



-- Layout ------------------------


columnAttributes : List (Attribute variation msg)
columnAttributes =
    [ spacing 20, padding 20 ]



-- Styles -------------------------


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
