module Client exposing (..)

import Types exposing (..)
import Styles exposing (..)
import Attributes exposing (columnAttributes)
import Table exposing (viewTable)
import TableMeta exposing (viewTableMeta, toggleValueForTable, modelWithTableMeta, toggleVariableSort, selectAll)
import Api exposing (loadSiteCmd, loadLevelCmd, submitQueryCmd)
import Contexts exposing (elementFromSite, columnFromLevelContext, modelWithSite, modelWithLevel)
import Config exposing (..)


-- External ---------------------

import Html exposing (Html)
import Element exposing (layout, row, column)
import Element.Attributes exposing (spread)


initialModel : Model
initialModel =
    { siteContext = { selected = swedish, sites = sites }
    , levelContexts = []
    , tableMeta = Nothing
    , table = Nothing
    , latestError = Nothing
    , showPlot = False
    }


view : Model -> Html Msg
view model =
    let
        language =
            model.siteContext.selected.language
    in
        layout stylesheet <|
            case model.tableMeta of
                Nothing ->
                    row Main
                        [ spread ]
                        ((column Styles.Site
                            columnAttributes
                            (List.map (elementFromSite model.siteContext.selected) sites)
                         )
                            :: (List.map columnFromLevelContext model.levelContexts)
                        )

                Just meta ->
                    case model.table of
                        Nothing ->
                            viewTableMeta meta language

                        Just table ->
                            viewTable table meta model.showPlot language


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

        ToggleSort variable ->
            case model.tableMeta of
                Just table ->
                    ( { model | tableMeta = Just <| toggleVariableSort variable table }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        TogglePlot ->
            ( { model | showPlot = not model.showPlot }, Cmd.none )

        SelectAll variable ->
            case model.tableMeta of
                Just table ->
                    ( { model | tableMeta = Just <| selectAll variable table }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, loadSiteCmd swedish )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
