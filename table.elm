module Table exposing (viewTable, viewTableMeta)

import Types exposing (..)
import Utils exposing (..)
import Styles exposing (..)
import Attributes exposing (..)
import Element exposing (..)
import Element.Attributes exposing (verticalCenter, spacing, padding, paddingRight, paddingXY, justify, yScrollbar, scrollbars, maxHeight, px)
import Element.Events exposing (onClick)


viewTable : TableData -> TableMeta -> Element Styles variation Msg
viewTable table meta =
    column Table
        columnAttributes
        [ row None
            [ justify ]
            [ el TableTitle [] <| text meta.title
            , (row None
                []
                [ button <| el Main [ onClick ToggleTableDataView ] <| text "X" ]
              )
            ]
        , viewValues table meta
        ]


viewValues : TableData -> TableMeta -> Element Styles variation Msg
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
            [ scrollbars ]
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
                |> Maybe.withDefault [ "*error*" ]
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
        dimCount =
            List.length dimensions

        pointSize : Int
        pointSize =
            data.points
                |> List.map .values
                |> List.map List.length
                |> List.head
                |> Maybe.withDefault 0

        values : List (Element.OnGrid (Element Styles variation msg))
        values =
            data.points
                |> List.indexedMap (viewDataPoint rowIndex dimCount pointSize)
                |> List.foldr (++) []
    in
        dimensions ++ values


viewDataPoint : Int -> Int -> Int -> Int -> DataPoint -> List (Element.OnGrid (Element Styles variation msg))
viewDataPoint rowIndex dimensionCount pointSize pointIndex point =
    let
        baseIndex =
            dimensionCount + pointIndex * pointSize
    in
        point.values
            |> List.indexedMap (viewValue rowIndex baseIndex)


viewValue : Int -> Int -> Int -> String -> Element.OnGrid (Element Styles variation msg)
viewValue rowIndex baseIndex valueIndex value =
    viewDataCell
        rowIndex
        (baseIndex + valueIndex)
        value


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
        (el style [ verticalCenter, scrollbars, padding 2 ] (text value))


emptyVariableMeta : VariableMeta
emptyVariableMeta =
    VariableMeta "" "" [] False

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