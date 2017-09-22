module Utils exposing (..)

import Types exposing (..)
import Json.Encode exposing (..)


mapIf : (a -> Bool) -> (a -> a) -> List a -> List a
mapIf pred map =
    List.map
        (\e ->
            if pred e then
                map e
            else
                e
        )

groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy eq xs_ =
  case xs_ of
    [] ->
        []
    (x::xs) ->
        let
            (ys,zs) = List.partition (eq x) xs
        in
            (x::ys)::groupBy eq zs

encodeQuery : Maybe TableMeta -> String
encodeQuery tableMeta =
    case tableMeta of
        Just table ->
            encode 2 <|
            object
                [ ( "query", query table )
                , ( "response", object [ ( "format", string "json" ) ] )
                ]

        Nothing ->
            ""


query : TableMeta -> Value
query table =
    list <| List.map variableQuery table.variables


variableQuery : VariableMeta -> Value
variableQuery variable =
    object
        [ ( "code", string variable.code )
        , ( "selection"
          , object
                [ ( "filter", string "item" )
                , ( "values"
                  , list
                        (variable.values
                            |> List.filter .selected
                            |> List.map .value
                            |> List.map string
                        )
                  )
                ]
          )
        ]
