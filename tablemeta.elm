module TableMeta exposing (viewTableMeta, toggleValueForTable, modelWithTableMeta)

import Types exposing (..)
import Styles exposing (..)
import Utils exposing (mapIf)
import Attributes exposing (columnAttributes, listAttributes)


-- External ------

import Element exposing (Element, text, button, el, column, row)
import Element.Attributes exposing (paddingRight, justify, yScrollbar, maxHeight, px)
import Element.Events exposing (onClick)


-- View -------


viewTableMeta : TableMeta -> Element Styles variation Msg
viewTableMeta meta =
    column Table
        columnAttributes
        [ row None
            [ justify ]
            [ el TableTitle [] <| text meta.title
            , (row None
                []
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
        [ el VariableName [ paddingRight 10 ] <| text variable.text
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
            if val.selected then
                Selected
            else
                None
    in
        el style [ onClick (ToggleValue var val) ] (text val.text)



-- Update -----------------


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
        values =
            variable.values
                |> mapIf (\val -> val.value == value.value)
                    (\val -> { val | selected = not val.selected })
    in
        { variable | values = values }
