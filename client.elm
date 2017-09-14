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
    { siteContext = { selected = swedish, sites = sites }
    , levelContexts = []
    }


type Msg
    = SelectSite Site
    | SiteLoaded Site (Result Http.Error (List Level))


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        row Main
            []
            ((column Main
                columnAttributes
                (List.map (elementFromSite model.siteContext.selected) sites)
             )
                :: (List.map columnFromLevelContext model.levelContexts)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectSite site ->
            ( model, loadSiteCmd site )

        SiteLoaded site (Ok levels) ->
            ( modelWithSite model site levels, Cmd.none )

        SiteLoaded _ (Err _) ->
            ( model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, loadSiteCmd swedish )
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


columnFromLevelContext : LevelCtx -> Element Styles variation Msg
columnFromLevelContext context =
    column Main
        columnAttributes
        (List.map (elementFromLevel context.selected) context.levels)


elementFromLevel : Maybe Level -> Level -> Element Styles variation Msg
elementFromLevel selected level =
    let
        style =
            case selected of
                Just sel ->
                    if sel == level then
                        Selected
                    else
                        None

                Nothing ->
                    None
    in
        el style [] (text level.text)


modelWithSite : Model -> Site -> List Level -> Model
modelWithSite model site levels =
    let
        oldSiteContext =
            model.siteContext

        oldLevelContexts =
            model.levelContexts
    in
        { siteContext = { oldSiteContext | selected = site }
        , levelContexts =
            if List.length levels > 0 then
                [ { index = 0
                  , selected = Nothing
                  , levels = levels
                  }
                ]
            else
                []
        }


loadSiteCmd : Site -> Cmd Msg
loadSiteCmd site =
    list levelDecoder
        |> Http.get site.url
        |> Http.send (SiteLoaded site)


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
