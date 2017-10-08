module DataPlot exposing (viewPlot, canPlot)

import Types exposing (..)
import Styles exposing (..)
import Attributes exposing (columnAttributes, buttonHeight)
import Elements exposing (buttonElement, titleRow)
import Translations exposing (translate)


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
import Svg.Attributes exposing (stroke, strokeDasharray, r, strokeWidth)
import Svg.Events exposing (onMouseOver, onMouseOut)
import Svg.Attributes exposing (stroke)
import Element exposing (Element, column, text, row, el, button, html, paragraph)
import Element.Attributes exposing (spread, height, fill, width, percent, spacing)
import Set exposing (Set)
import Array exposing (Array)


-- Can plot? ------------------------------------


canPlot : List DataSequence -> List Column -> Bool
canPlot data columns =
    let
        isAllNumeric =
            data
                |> List.all isNumericSequence

        onlyHasSingleDataPoints =
            (columns
                |> List.map .type_
                |> List.filter ((==) "c")
                |> List.length
            )
                == 1
    in
        onlyHasSingleDataPoints && isAllNumeric


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



-- View -----------------------------


viewPlot : List DataSequence -> TableMeta -> Bool -> Maybe DataPoint -> String -> Element Styles variation Msg
viewPlot data meta plotFromYAtZero hoverPoint language =
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

        keys : List (List String)
        keys =
            List.map .key data

        subKeyIndices : List Int
        subKeyIndices =
            subKeysToUse keys
    in
        column Table
            columnAttributes
            [ titleRow meta.title
                [ buttonElement (translate ToggleOriginKey language) TogglePlotOrigo True
                , buttonElement "X" TogglePlot True
                ]
            , row None
                [ spacing 20 ]
                [ el Main [ height fill, width (percent 70) ] <| html <| plotDataSequences data times plotFromYAtZero
                , column None [spacing 20] 
                    [ legend data subKeyIndices
                    , showPoint hoverPoint
                    ]
                ]
            ]


subKeysToUse : List (List String) -> List Int
subKeysToUse keys =
    let
        keyLength : Int
        keyLength =
            keys
                |> List.head
                |> Maybe.withDefault []
                |> List.length

        emptySets : List (Set String)
        emptySets =
            List.repeat keyLength Set.empty

        combinedKeys : List (Set String)
        combinedKeys =
            List.foldl combineKeys emptySets keys
    in
        combinedKeys
            |> List.indexedMap
                (\i s ->
                    if Set.size s == 1 then
                        Nothing
                    else
                        Just i
                )
            |> List.filterMap identity


combineKeys : List String -> List (Set String) -> List (Set String)
combineKeys key combined =
    List.map2 (\k s -> Set.insert k s) key combined


legend : List DataSequence -> List Int -> Element Styles variation msg
legend data subKeyIndices =
    column None [ spacing 5 ] <|
        List.indexedMap (legendLabel subKeyIndices) data


legendLabel : List Int -> Int -> DataSequence -> Element Styles variation msg
legendLabel subKeyIndices index data =
    let
        keyArray =
            data.key
                |> Array.fromList

        key =
            subKeyIndices
                |> List.filterMap (\i -> Array.get i keyArray)
                |> List.foldl (\k ks -> ks ++ "[" ++ k ++ "] ") ""
                |> String.trim
    in
        paragraph (colourForIndex index) [] [ text key ]


plotDataSequences : List DataSequence -> List String -> Bool -> Svg Msg
plotDataSequences dataSeqs times plotFromYAtZero =
    let
        domainLowest : Float -> Float
        domainLowest =
            if plotFromYAtZero then
                always 0
            else
                identity
    in
        viewSeriesCustom
            { defaultSeriesPlotCustomizations
                | horizontalAxis = timeAxis times
                , margin =
                    { top = 20
                    , right = 40
                    , bottom = 30
                    , left = 80
                    }
                , toDomainLowest = domainLowest
            }
            (dataSeqs
                |> List.indexedMap plotLine
            )
            dataSeqs


plotLine : Int -> DataSequence -> Series (List DataSequence) Msg
plotLine seriesIndex seq =
    let
        colour =
            colourNameForIndex seriesIndex
    in
        customSeries normalAxis (Plot.Linear Nothing [ stroke colour ]) (\seqs -> preparePoints colour seq.points)


preparePoints : String -> List DataPoint -> List (Plot.DataPoint Msg)
preparePoints colour points =
    points
        |> List.indexedMap (plotPoint colour)
        |> List.filterMap identity


plotPoint : String -> Int -> DataPoint -> Maybe (Plot.DataPoint Msg)
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
                Just (dot (circle point colour) (toFloat index) val)

            Err err ->
                Nothing

circle : DataPoint -> String -> Svg Msg
circle point colour =
  Svg.circle
    [ r "5"
    , stroke "transparent"
    , strokeWidth "3px"
    , Svg.Attributes.fill colour
    , onMouseOver (HoverPoint (Just point))
    , onMouseOut (HoverPoint Nothing)
    ]
    []


colourNames : List String
colourNames =
    [ "fuchsia", "red", "blue", "green", "maroon", "purple", "aqua", "olive" ]


colourStyles : List Styles.Styles
colourStyles =
    [ PlotFuchsia
    , PlotRed
    , PlotBlue
    , PlotGreen
    , PlotMaroon
    , PlotPurple
    , PlotAqua
    , PlotOlive
    ]


colourForIndex : Int -> Styles.Styles
colourForIndex index =
    colourStyles
        |> List.drop index
        |> List.head
        |> Maybe.withDefault PlotBlack


colourNameForIndex : Int -> String
colourNameForIndex index =
    colourNames
        |> List.drop index
        |> List.head
        |> Maybe.withDefault "black"


timeAxis : List String -> Axis
timeAxis times =
    let
        tickIndices =
            reasonableTicks times
    in
        customAxis <|
            \summary ->
                { position = closestToZero
                , axisLine = Just (simpleLine summary)
                , ticks = ticks tickIndices
                , labels = labels times tickIndices
                , flipAnchor = False
                }


ticks : List Int -> List TickCustomizations
ticks indices =
    indices
        |> List.map toFloat
        |> List.map simpleTick


reasonableTicks : List String -> List Int
reasonableTicks times =
    let
        length =
            List.length times

        step : Int
        step =
            max 1 ((length - 1) // 5 + 1)
    in
        List.range 0 (length - 1)
            |> List.map ((*) step)
            |> List.filter ((>) length)


labels : List String -> List Int -> List LabelCustomizations
labels times indices =
    times
        |> List.indexedMap
            (\i t ->
                if List.member i indices then
                    Just ( i, t )
                else
                    Nothing
            )
        |> List.filterMap identity
        |> List.map (\( i, t ) -> axisLabel i t)


axisLabel : Int -> String -> LabelCustomizations
axisLabel pos txt =
    { view = viewLabel [] txt
    , position = toFloat pos
    }


emptyVariableMeta : VariableMeta
emptyVariableMeta =
    VariableMeta "" "" [] False False

showPoint : Maybe DataPoint -> Element Styles variation msg
showPoint point =
    case point of
        Just p ->
            el None [] <| paragraph None [] [text <| pointToText p]
        Nothing ->
            text ""

pointToText : DataPoint -> String
pointToText point =
    "{ " ++ point.time ++ " => " ++ valueOfPoint point ++ " }"

valueOfPoint : DataPoint -> String
valueOfPoint point =
    point.values
        |> List.head
        |> Maybe.withDefault ""