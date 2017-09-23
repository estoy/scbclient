module Client exposing (..)

import Types exposing (..)
import Utils exposing (..)

import Json.Decode exposing (string, list, Decoder, at, map, lazy, oneOf, null, bool)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, custom, optional)
import Http exposing (stringBody, Body, Request, request, expectJson, header)
import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (verticalCenter, spacing, padding, paddingRight, paddingXY, justify, yScrollbar, scrollbars, maxHeight, px)
import Element.Events exposing (onClick)
import Color
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Border as Border


initialModel : Model
initialModel =
    { siteContext = { selected = swedish, sites = sites }
    , levelContexts = []
    , tableMeta = Nothing
    , table = Nothing
    , latestError = Nothing
    }


type Msg
    = SelectSite Site
    | SiteLoaded Site (Result Http.Error (List Level))
    | SelectLevel Level Int
    | LevelLoaded Level Int (Result Http.Error (List Level))
    | TableMetaLoaded Level Int (Result Http.Error TableMeta)
    | ToggleTableMetaView
    | ToggleTableDataView
    | ToggleValue VariableMeta ValueMeta
    | Submit
    | TableLoaded (Result Http.Error TableData)


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        case model.tableMeta of
            Nothing ->
                row Main
                    []
                    ((column Site
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
                

viewTable : TableData -> TableMeta -> Element Styles variation Msg
viewTable table meta =
    column Table
        columnAttributes
        [ row None
            [ justify ]
            [ el TableTitle [] <| text meta.title
            , (row None []
                [button <| el Main [ onClick ToggleTableDataView ] <| text "X"]
              )
            ]
        , viewValues table meta
        ]

viewValues : TableData -> TableMeta-> Element Styles variation Msg
viewValues table meta =
   let
        timeField : VariableMeta
        timeField =
            meta.variables
                |> List.filter .time
                |> List.head
                |> Maybe.withDefault emptyVariableMeta
        
        timeCount =
            timeField.values
                |> List.filter .selected
                |> List.length

        dimensionCount =
            table.columns
                |> List.map .type_
                |> List.filter ((==) "d")
                |> List.length

        dataCount =
            table.columns
                |> List.map .type_
                |> List.filter ((==) "c")
                |> List.length

        columnCount =
            timeCount * dataCount + dimensionCount

        dataSeqs : List DataSequence
        dataSeqs =
            table.data
                |> groupBy .key
                |> List.map mergeSequences
                |> List.map (lookupKey meta.variables)

        rowCount =
            dataSeqs
                |> List.length

   in
        grid DataGrid
            { columns = List.repeat columnCount (px 150)
            , rows = List.repeat rowCount (px 34)
            }
            [scrollbars]
            (dataSeqs
                |> List.indexedMap viewDataRow
                |> List.foldr (++) []
            )

lookupKey : List VariableMeta -> DataSequence -> DataSequence
lookupKey variables seq =
    let
        translatedKey : List String
        translatedKey =
            List.map2 lookupVariable variables seq.key
    in
        { seq | key = translatedKey }

lookupVariable : VariableMeta -> String -> String
lookupVariable meta var =
    meta.values
        |> List.filter (\val -> val.value == var)
        |> List.map .text
        |> List.head
        |> Maybe.withDefault "*error*"

mergeSequences : List Data -> DataSequence
mergeSequences group =
    let
        points : List DataPoint
        points =
            group
                |> List.map (\data -> DataPoint data.time data.values)

        key =
            group
                |> List.map .key
                |> List.head
                |> Maybe.withDefault ["*error*"]
    in
        DataSequence key points


viewDataRow : Int -> DataSequence -> List (Element.OnGrid (Element Styles variation msg))
viewDataRow rowIndex data =
    let
        dimensions : List (Element.OnGrid (Element Styles variation msg))
        dimensions =
            data.key
                |> List.indexedMap (viewDimensionCell rowIndex)

        dimCount : Int
        dimCount = List.length dimensions

        pointSize : Int
        pointSize = data.points
                        |> List.map .values
                        |> List.map List.length
                        |> List.head
                        |> Maybe.withDefault 0

        values : List (Element.OnGrid (Element Styles variation msg))
        values =
            data.points
                |> List.indexedMap (\pindex point ->
                        List.indexedMap (\vindex value -> viewDataCell
                                                            rowIndex 
                                                            (dimCount + pindex * pointSize + vindex)
                                                            value
                                        )
                                        point.values
                                    )
                |> List.foldr (++) []
    in
        dimensions ++ values

viewDimensionCell : Int -> Int -> String -> Element.OnGrid (Element Styles variation msg)
viewDimensionCell rowIndex columnIndex value =
    viewCell DimBox rowIndex columnIndex value

viewDataCell : Int -> Int -> String -> Element.OnGrid (Element Styles variation msg)
viewDataCell rowIndex columnIndex value =
    viewCell DataBox rowIndex columnIndex value

viewCell : Styles -> Int -> Int -> String -> Element.OnGrid (Element Styles variation msg)
viewCell style rowIndex columnIndex value =
    area
        { start = ( columnIndex, rowIndex )
        , width = 1
        , height = 1
        }
        (el style [verticalCenter, scrollbars, padding 2] (text value))

emptyVariableMeta : VariableMeta
emptyVariableMeta = VariableMeta "" "" [] False

viewTableMeta : TableMeta -> Element Styles variation Msg
viewTableMeta meta =
    column Table
        columnAttributes
        [ row None
            [ justify ]
            [ el TableTitle [] <| text meta.title
            , (row None []
                [ button <| el Main [ onClick Submit ] <| text "Submit"
                , button <| el Main [ onClick ToggleTableMetaView ] <| text "X"
                ]
              )
            ]
        , viewVariablesMeta meta.variables
        ]


viewVariablesMeta : List VariableMeta -> Element Styles variation Msg
viewVariablesMeta variables =
    column None columnAttributes <|
        List.map viewVariableMeta variables


viewVariableMeta : VariableMeta -> Element Styles variation Msg
viewVariableMeta variable =
    row None
        []
        [ el VariableName [paddingRight 10] <| text variable.text
        , column VariableData
            ([ yScrollbar, maxHeight (px 150) ] ++ listAttributes)
            (variable.values
                |> List.map (viewValueMeta variable)
            )
        ]

viewValueMeta : VariableMeta -> ValueMeta -> Element Styles variation Msg
viewValueMeta var val =
    let
        style =
            if val.selected then Selected else None        
    in
        el style [onClick (ToggleValue var val)] (text val.text)
            

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
                    ( model, Cmd.none)

        Submit ->
            ( model, submitQueryCmd model)
        
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
                None
    in
        el style [ onClick <| SelectSite site ] (text site.language)


columnFromLevelContext : LevelCtx -> Element Styles variation Msg
columnFromLevelContext context =
    let
        style =
            if
                List.any (\level -> level.type_ == "t") context.levels
            then
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
        values = variable.values
                    |> mapIf (\val -> val.value == value.value)
                             (\val -> { val | selected = not val.selected})
    in
        { variable | values = values }

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
    decode VariableMetaDTO 
        |> required "code" string
        |> required "text" string
        |> required "values" (list string)
        |> required "valueTexts" (list string)
        |> optional "time" bool False
        |> Json.Decode.map prepareValues

prepareValues : VariableMetaDTO -> VariableMeta
prepareValues dto =
    let
        values : List ValueMeta
        values = List.map2 (\value text -> 
            { value = value,
            text = text,
            selected = False})
            dto.values
            dto.valueTexts
    in
        { code = dto.code
        , text = dto.text
        , values = values
        , time = dto.time }
        
submitQueryCmd : Model -> Cmd Msg
submitQueryCmd model =
    let
        url =
            tableUrl model
        query =
            tableQuery model
    in
        tableDecoder
            |> Http.post url query
            |> Http.send TableLoaded

tableDecoder : Decoder TableData
tableDecoder =
    decode TableData
        |> required "data" (list dataDecoder)
        |> required "columns" (list columnDecoder)

columnDecoder : Decoder Column
columnDecoder =
    decode Column
        |> required "code" string
        |> required "text" string
        |> required "type" string

dataDecoder : Decoder Data
dataDecoder =
    decode DataDTO
        |> required "key" (list string)
        |> required "values" (list string)
        |> Json.Decode.map prepareData

prepareData : DataDTO -> Data
prepareData dto =
    let
        key : List String
        key = dto.key
                |> List.take (List.length dto.key - 1)

        time : String
        time = dto.key
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""
    in
        { key = key
        , time = time
        , values = dto.values
        }

tableUrl : Model -> String
tableUrl model =
    Debug.log "Url:" (model.siteContext.selected.url ++ pathForTable model.levelContexts)

pathForTable : List LevelCtx -> String
pathForTable contexts =
    contexts
        |> List.map currentId
        |> List.foldl (\a b -> b ++ "/" ++ a) ""

tableQuery : Model -> Http.Body
tableQuery model =
    stringBody "application/json" <| encodeQuery model.tableMeta


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


listAttributes : List (Attribute variation msg)
listAttributes =
    [ spacing 5, paddingXY 10 0 ]



-- Styles -------------------------


type Styles
    = None
    | Main
    | Selected
    | Site
    | Table
    | TableTitle
    | VariableName
    | VariableData
    | DimBox
    | DataBox
    | DataGrid

tableBackground = Color.rgba 231 214 166 1.0
dataBackground = Color.rgba 239 227 195 1.0

baseStyle : List (Style.Property class variation)
baseStyle =
    [Font.typeface [ "helvetica", "arial", "sans-serif" ]
    , Font.size 16
    , Font.lineHeight 1.3
    ]

stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Main
            ( [ Color.text Color.darkCharcoal
              , Color.background Color.lightGrey
              ]
              ++ baseStyle
            )            
        , style Selected
            [ Color.text Color.white
            , Color.background Color.charcoal
            ]
        , style Site
            [ Color.background (Color.rgba 186 196 238 1.0) ]
        , style Table
            ( [ Color.text Color.darkCharcoal
              , Color.background tableBackground
              ]
              ++ baseStyle
            )
        , style TableTitle
            [Font.size 24, Font.bold]
        , style VariableName
            [Font.bold]
        , style VariableData
            [Color.background dataBackground]
        , style DimBox
            [ Border.all 1.0
            , Font.size 12
            , Font.lineHeight 1.2
            ]
        , style DataBox
            [ Border.all 1.0
            , Font.size 12
            , Font.lineHeight 1.2
            , Color.background (Color.rgba 186 196 238 1.0)
            ]
        , style DataGrid
            [ Color.background dataBackground ]
        ]


