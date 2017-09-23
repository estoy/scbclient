module Api exposing (..)

import Types exposing (..)
import Utils exposing (..)

import Json.Decode exposing (string, list, Decoder, at, map, lazy, oneOf, null, bool)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, custom, optional)
import Http exposing (stringBody, Body, Request, request, expectJson, header)

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