module Client exposing (..)

import Types exposing (..)
import Utils exposing (..)
import Styles exposing (..)
import Attributes exposing (..)
import Table exposing (..)
import Api exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Element.Events exposing (onClick)


initialModel : Model
initialModel =
    { siteContext = { selected = swedish, sites = sites }
    , levelContexts = []
    , tableMeta = Nothing
    , table = Nothing
    , latestError = Nothing
    }


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        case model.tableMeta of
            Nothing ->
                row Main
                    []
                    ((column Styles.Site
                        columnAttributes
                        (List.map (elementFromSite model.siteContext.selected) sites)
                     )
                        :: (List.map columnFromLevelContext model.levelContexts)
                    )

            Just meta ->
                case model.table of
                    Nothing ->
                        viewTableMeta meta

                    Just table ->
                        viewTable table meta


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

        ToggleTableMetaView ->
            ( { model | tableMeta = Nothing }, Cmd.none )

        ToggleTableDataView ->
            ( { model | table = Nothing }, Cmd.none )

        ToggleValue variable value ->
            case model.tableMeta of
                Just table ->
                    ( { model | tableMeta = Just <| toggleValueForTable variable value table }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Submit ->
            ( model, submitQueryCmd model )

        TableLoaded (Ok table) ->
            ( { model | table = Just table }, Cmd.none )

        TableLoaded (Err err) ->
            ( { model | latestError = Just err }, Cmd.none )


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
                Deselected
    in
        el style [ onClick <| SelectSite site ] (text site.language)


columnFromLevelContext : LevelCtx -> Element Styles variation Msg
columnFromLevelContext context =
    let
        style =
            if List.any (\level -> level.type_ == "t") context.levels then
                Table
            else
                Main
    in
        column style
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
                        Deselected

                Nothing ->
                    Deselected
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


toggleValueForTable : VariableMeta -> ValueMeta -> TableMeta -> TableMeta
toggleValueForTable variable value table =
    let
        variables =
            table.variables
                |> mapIf (\var -> var.code == variable.code) (toggleValueForVar value)
    in
        { table | variables = variables }


toggleValueForVar : ValueMeta -> VariableMeta -> VariableMeta
toggleValueForVar value variable =
    let
        values =
            variable.values
                |> mapIf (\val -> val.value == value.value)
                    (\val -> { val | selected = not val.selected })
    in
        { variable | values = values }


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
