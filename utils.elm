module Utils exposing (mapIf, groupBy, encodeQuery)

import Types exposing (..)


-- External ----------

import Json.Encode exposing (encode, object, string, list, Value)


mapIf : (a -> Bool) -> (a -> a) -> List a -> List a
mapIf pred map =
    List.map
        (\e ->
            if pred e then
                map e
            else
                e
        )


groupBy : (a -> b) -> List a -> List (List a)
groupBy key xs_ =
    let
        eq =
            \x y -> (key x) == (key y)
    in
        case xs_ of
            [] ->
                []

            x :: xs ->
                let
                    ( ys, zs ) =
                        List.partition (eq x) xs
                in
                    (x :: ys) :: groupBy key zs


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
