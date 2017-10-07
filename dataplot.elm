module DataPlot exposing (viewPlot)

import Types exposing (..)
import Styles exposing (..)
import Attributes exposing (columnAttributes)


-- External ------

import Plot
    exposing
        ( customSeries
        , Axis
        , customAxis
        , normalAxis
        , dot
        , viewCircle
        , viewSeries
        , Series
        , closestToZero
        , simpleLine
        , simpleTick
        , simpleLabel
        , TickCustomizations
        , LabelCustomizations
        , viewSeriesCustom
        , defaultSeriesPlotCustomizations
        , viewLabel
        )
import Svg exposing (Svg)
import Svg.Attributes exposing (stroke)
import Element exposing (Element, column, text, row, el, button, html)
import Element.Attributes exposing (spread, height, fill)
import Element.Events exposing (onClick)


viewPlot : List DataSequence -> TableMeta -> Element Styles variation Msg
viewPlot data meta =
    let
        times : List String
        times =
            meta.variables
                |> List.filter .time
                |> List.head
                |> Maybe.withDefault emptyVariableMeta
                |> .values
                |> List.filter .selected
                |> List.map .text
    in
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
            , el Main [ height fill ] <| html <| plotDataSequences data times
            ]


plotDataSequences : List DataSequence -> List String -> Svg msg
plotDataSequences dataSeqs times =
    viewSeriesCustom
        { defaultSeriesPlotCustomizations
            | horizontalAxis = timeAxis times
            , margin =
                { top = 20
                , right = 40
                , bottom = 80
                , left = 40
                }
        }
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
        customSeries normalAxis (Plot.Linear Nothing [ stroke colour ]) (\seqs -> preparePoints colour seq.points)


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


colours : List String
colours =
    [ "fuchsia", "red", "blue", "green", "maroon", "purple", "aqua", "olive" ]


timeAxis : List String -> Axis
timeAxis times =
    customAxis <|
        \summary ->
            { position = closestToZero
            , axisLine = Just (simpleLine summary)
            , ticks = ticks <| List.length times
            , labels = List.indexedMap axisLabel times
            , flipAnchor = False
            }


ticks : Int -> List TickCustomizations
ticks count =
    List.range 1 count
        |> List.map toFloat
        |> List.map simpleTick


axisLabel : Int -> String -> LabelCustomizations
axisLabel pos txt =
    { view = viewLabel [] txt
    , position = toFloat pos
    }


emptyVariableMeta : VariableMeta
emptyVariableMeta =
    VariableMeta "" "" [] False False
