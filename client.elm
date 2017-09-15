module Client exposing (..)

import Types exposing (..)
import Json.Decode exposing (string, list, Decoder, at, map, lazy, oneOf, null)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, custom, optional)
import Http
import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (spacing, padding, justify)
import Element.Events exposing (onClick)
import Color
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font


initialModel : Model
initialModel =
    { siteContext = { selected = swedish, sites = sites }
    , levelContexts = []
    , tableMeta = Nothing
    , latestError = Nothing
    }


type Msg
    = SelectSite Site
    | SiteLoaded Site (Result Http.Error (List Level))
    | SelectLevel Level Int
    | LevelLoaded Level Int (Result Http.Error (List Level))
    | TableMetaLoaded Level Int (Result Http.Error TableMeta)
    | ToggleTableView


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        case model.tableMeta of
            Nothing ->
                row Main
                    []
                    ((column Main
                        columnAttributes
                        (List.map (elementFromSite model.siteContext.selected) sites)
                     )
                        :: (List.map columnFromLevelContext model.levelContexts)
                    )

            Just meta ->
                viewTableMeta meta


viewTableMeta : TableMeta -> Element Styles variation Msg
viewTableMeta meta =
    column Main
        columnAttributes
        [ row Main
            [ justify ]
            [ text <| .title meta
            , button <| el Main [ onClick ToggleTableView ] <| text "X"
            ]
        , viewVariableMeta meta.variables
        ]


viewVariableMeta : List VariableMeta -> Element Styles variation msg
viewVariableMeta variables =
    column Main
        columnAttributes
        (variables
            |> List.map .text
            |> List.map text
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

        SelectLevel level index ->
            ( model, loadLevelCmd level index model )

        LevelLoaded level index (Ok levels) ->
            ( modelWithLevel model level index levels, Cmd.none )

        LevelLoaded _ _ (Err err) ->
            ( { model | latestError = Just err }, Cmd.none )

        TableMetaLoaded level index (Ok tableMeta) ->
            ( modelWithTableMeta model level index tableMeta, Cmd.none )

        TableMetaLoaded _ _ (Err err) ->
            ( { model | latestError = Just err }, Cmd.none )

        ToggleTableView ->
            ( { model | tableMeta = Nothing }, Cmd.none )


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
        (List.map (elementFromLevel context.selected context.index) context.levels)


elementFromLevel : Maybe Level -> Int -> Level -> Element Styles variation Msg
elementFromLevel selected index level =
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
        el style [ onClick <| SelectLevel level index ] (text level.text)


modelWithSite : Model -> Site -> List Level -> Model
modelWithSite model site levels =
    let
        oldSiteContext =
            model.siteContext

        oldLevelContexts =
            model.levelContexts
    in
        { model
            | siteContext = { oldSiteContext | selected = site }
            , levelContexts =
                if List.length levels > 0 then
                    [ { index = 0
                      , selected = Nothing
                      , levels = levels
                      }
                    ]
                else
                    []
            , tableMeta = Nothing
        }


modelWithLevel : Model -> Level -> Int -> List Level -> Model
modelWithLevel model level index levels =
    let
        parentContexts =
            List.take index model.levelContexts

        newContext =
            { index = index + 1
            , selected = Nothing
            , levels = levels
            }

        selectedContext =
            model.levelContexts
                |> List.take (index + 1)
                |> List.reverse
                |> List.head

        updatedContext =
            case selectedContext of
                Just ctx ->
                    { ctx | selected = Just level }

                Nothing ->
                    { index = index, selected = Nothing, levels = [] }
    in
        { model
            | levelContexts = parentContexts ++ [ updatedContext, newContext ]
            , tableMeta = Nothing
        }


modelWithTableMeta : Model -> Level -> Int -> TableMeta -> Model
modelWithTableMeta model level index tableMeta =
    let
        parentContexts =
            List.take index model.levelContexts

        selectedContext =
            model.levelContexts
                |> List.take (index + 1)
                |> List.reverse
                |> List.head

        updatedContext =
            case selectedContext of
                Just ctx ->
                    { ctx | selected = Just level }

                Nothing ->
                    { index = index, selected = Nothing, levels = [] }
    in
        { model
            | levelContexts = parentContexts ++ [ updatedContext ]
            , tableMeta = Just tableMeta
        }


loadSiteCmd : Site -> Cmd Msg
loadSiteCmd site =
    list levelDecoder
        |> Http.get site.url
        |> Http.send (SiteLoaded site)


loadLevelCmd : Level -> Int -> Model -> Cmd Msg
loadLevelCmd level index model =
    let
        url =
            urlForLevel model level index
    in
        case level.type_ of
            "l" ->
                list levelDecoder
                    |> Http.get url
                    |> Http.send (LevelLoaded level index)

            "t" ->
                tableMetaDecoder
                    |> Http.get url
                    |> Http.send (TableMetaLoaded level index)

            _ ->
                Cmd.none


levelDecoder : Decoder Level
levelDecoder =
    decode Level
        |> required "id" string
        |> required "type" string
        |> required "text" string


tableMetaDecoder : Decoder TableMeta
tableMetaDecoder =
    decode TableMeta
        |> required "title" string
        |> required "variables" (list variableMetaDecoder)


variableMetaDecoder : Decoder VariableMeta
variableMetaDecoder =
    decode VariableMeta
        |> required "code" string
        |> required "text" string
        |> required "values" (list string)
        |> required "valueTexts" (list string)


urlForLevel : Model -> Level -> Int -> Url
urlForLevel model level index =
    Debug.log "Url:" (model.siteContext.selected.url ++ pathForLevel model.levelContexts level index)


pathForLevel : List LevelCtx -> Level -> Int -> String
pathForLevel contexts level index =
    let
        parentPath =
            contexts
                |> List.take (index)
                |> List.map currentId
                |> List.foldl (\a b -> b ++ "/" ++ a) ""
    in
        parentPath ++ "/" ++ level.id


currentId : LevelCtx -> String
currentId ctx =
    case ctx.selected of
        Just level ->
            level.id

        option2 ->
            ""


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


emptyTableMeta : TableMeta
emptyTableMeta =
    { title = "(no table selected)", variables = [] }



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
