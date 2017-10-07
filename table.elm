module Table exposing (viewTable)

import Types exposing (..)
import Styles exposing (..)
import Utils exposing (groupBy)
import Attributes exposing (columnAttributes)


-- External ------

import Element exposing (Element, paragraph, column, text, row, el, button, grid, cell, html)
import Element.Attributes exposing (spacing, verticalCenter, padding, spread, px, scrollbars, height, fill)
import Element.Events exposing (onClick)
import Plot exposing (customSeries, normalAxis, dot, viewCircle, viewSeries, Series)
import Svg exposing (Svg)
import Svg.Attributes exposing (stroke)


-- View ---------------


viewTable : TableData -> TableMeta -> Bool -> Element Styles variation Msg
viewTable table meta showPlot =
    let
        data : List DataSequence
        data =
            dataSequences table meta

        isAllNumeric =
            data
                |> List.all isNumericSequence

        onlyHasSingleDataPoints =
            (table.columns
                |> List.map .type_
                |> List.filter ((==) "c")
                |> List.length)
            == 1 

        canPlot =
            onlyHasSingleDataPoints && isAllNumeric

        ( plotButtonStyle, plotButtonAttributes ) =
            if canPlot then
                ( Main, [ onClick TogglePlot ] )
            else
                ( Disabled, [] )
    in
        case showPlot of
            False ->
                column Table
                    columnAttributes
                    [ row None
                        [ spread ]
                        [ el TableTitle [] <| text meta.title
                        , (row None
                            [spacing 5]
                            [ button plotButtonStyle plotButtonAttributes <| text "Plot"
                            , button Main [ onClick ToggleTableDataView ] <| text "X"
                            ]
                          )
                        ]
                    , viewValues table meta
                    ]

            True ->
                column Table
                    columnAttributes
                    [ row None
                        [ spread ]
                        [ el TableTitle [] <| text meta.title
                        , (row None
                            []
                            [ button Main [ onClick TogglePlot ] <| text "X"
                            ]
                          )
                        ]
                    , el Main [ height fill ] <| html <| plotDataSequences data
                    ]


plotDataSequences : List DataSequence -> Svg msg
plotDataSequences dataSeqs =
    viewSeries
        (dataSeqs
            |> List.indexedMap plotLine
        )
        dataSeqs


plotLine : Int -> DataSequence -> Series (List DataSequence) msg
plotLine seriesIndex seq =
    let
        colour =
            colours
                |> List.drop seriesIndex
                |> List.head
                |> Maybe.withDefault "black"
    in 
        customSeries normalAxis (Plot.Linear Nothing [stroke colour]) (\seqs -> preparePoints colour seq.points)

preparePoints : String -> List DataPoint -> List (Plot.DataPoint msg)
preparePoints colour points =
    points
        |> List.indexedMap (plotPoint colour)
        |> List.filterMap identity

plotPoint : String -> Int -> DataPoint -> Maybe (Plot.DataPoint msg)
plotPoint colour index point =
    let
        value : String
        value =
            point.values
                |> List.head
                |> Maybe.withDefault ""

        floatValue : Result String Float
        floatValue =
            String.toFloat value
    in
        case floatValue of
            Ok val ->
                Just (dot (viewCircle 5.0 colour) (toFloat index) val)
            Err err ->
                Nothing


viewValues : TableData -> TableMeta -> Element Styles variation Msg
viewValues table meta =
    let
        timeField : VariableMeta
        timeField =
            meta.variables
                |> List.filter .time
                |> List.head
                |> Maybe.withDefault emptyVariableMeta

        timeValues : List ValueMeta
        timeValues =
            timeField.values
                |> List.filter .selected

        timeCount =
            timeValues
                |> List.length

        dimensionCount =
            table.columns
                |> List.map .type_
                |> List.filter ((==) "d")
                |> List.length

        dataColumns : List Column
        dataColumns =
            table.columns
                |> List.filter (\c -> c.type_ == "c")

        dataCount =
            dataColumns
                |> List.length

        columnCount =
            timeCount * dataCount + dimensionCount

        dataSeqs : List DataSequence
        dataSeqs =
            dataSequences table meta

        dimensionHeaders : List (Element.OnGrid (Element Styles variation msg))
        dimensionHeaders =
            meta.variables
                |> List.take dimensionCount
                |> List.indexedMap viewDimensionHeader

        dataHeaders : List (Element.OnGrid (Element Styles variation msg))
        dataHeaders =
            dataColumns
                |> List.repeat timeCount
                |> List.foldr (++) []
                |> List.indexedMap (viewDataHeader dimensionCount)

        timeHeaders : List (Element.OnGrid (Element Styles variation msg))
        timeHeaders =
            timeValues
                |> List.map .text
                |> List.indexedMap (viewTimeHeader dataCount dimensionCount)

        dataRowCount =
            dataSeqs
                |> List.length

        dataRows : List (Element.OnGrid (Element Styles variation msg))
        dataRows =
            dataSeqs
                |> List.indexedMap viewDataRow
                |> List.foldr (++) []
    in
        grid DataGrid
            [ scrollbars ]
            { columns = List.repeat columnCount (px 150)
            , rows = List.repeat (2 + dataRowCount) (px 34)
            , cells = dimensionHeaders ++ dataHeaders ++ timeHeaders ++ dataRows
            }


isNumericSequence : DataSequence -> Bool
isNumericSequence sequence =
    sequence.points
        |> List.all isNumericDataPoint


isNumericDataPoint : DataPoint -> Bool
isNumericDataPoint point =
    point.values
        |> List.all isNumericOrEmpty


isNumericOrEmpty : String -> Bool
isNumericOrEmpty str =
    let
        isEmpty =
            str == ""

        isPlaceHolder =
            str == ".."

        isNumeric =
            case String.toFloat str of
                Ok float ->
                    True

                Err err ->
                    False
    in
        isEmpty || isPlaceHolder || isNumeric


dataSequences : TableData -> TableMeta -> List DataSequence
dataSequences table meta =
    table.data
        |> groupBy .key
        |> List.map mergeSequences
        |> List.map (lookupKey meta.variables)


viewTimeHeader : Int -> Int -> Int -> String -> Element.OnGrid (Element Styles variation msg)
viewTimeHeader width baseIndex columnIndex text =
    viewCell HeaderBox 0 (baseIndex + columnIndex * width) text width


viewDataHeader : Int -> Int -> Column -> Element.OnGrid (Element Styles variation msg)
viewDataHeader baseIndex columnIndex column =
    viewCell HeaderBox 1 (baseIndex + columnIndex) column.text 1


viewDimensionHeader : Int -> VariableMeta -> Element.OnGrid (Element Styles variation msg)
viewDimensionHeader columnIndex var =
    viewCell HeaderBox 1 columnIndex var.text 1


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
viewDataRow dataRowIndex data =
    let
        rowIndex =
            dataRowIndex + 2

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
    viewCell DimBox rowIndex columnIndex value 1


viewDataCell : Int -> Int -> String -> Element.OnGrid (Element Styles variation msg)
viewDataCell rowIndex columnIndex value =
    viewCell DataBox rowIndex columnIndex value 1


viewCell : Styles -> Int -> Int -> String -> Int -> Element.OnGrid (Element Styles variation msg)
viewCell style rowIndex columnIndex value width =
    cell
        { start = ( columnIndex, rowIndex )
        , width = width
        , height = 1
        , content = el style [ verticalCenter, scrollbars, padding 2 ] (paragraph None [] [ text value ])
        }


emptyVariableMeta : VariableMeta
emptyVariableMeta =
    VariableMeta "" "" [] False False

colours : List String
colours =
    ["fuchsia", "red", "blue", "green", "maroon", "purple", "aqua", "olive"]