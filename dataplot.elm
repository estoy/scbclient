module DataPlot exposing (viewPlot)

import Types exposing (..)
import Styles exposing (..)
import Attributes exposing (columnAttributes)


-- External ------

import Plot exposing (customSeries, normalAxis, dot, viewCircle, viewSeries, Series)
import Svg exposing (Svg)
import Svg.Attributes exposing (stroke)
import Element exposing (Element, column, text, row, el, button, html)
import Element.Attributes exposing (spread, height, fill)
import Element.Events exposing (onClick)

viewPlot : List DataSequence -> TableMeta -> Element Styles variation Msg
viewPlot data meta =
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


colours : List String
colours =
    ["fuchsia", "red", "blue", "green", "maroon", "purple", "aqua", "olive"]