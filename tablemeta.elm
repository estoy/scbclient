module TableMeta exposing (viewTableMeta, toggleValueForTable, modelWithTableMeta, toggleVariableSort)

import Types exposing (..)
import Styles exposing (..)
import Utils exposing (mapIf)
import Attributes exposing (columnAttributes, listAttributes,  buttonHeight, titleAttributes)
import Elements exposing (titleElement)


-- External ------

import Element exposing (Element, text, button, el, column, row)
import Element.Attributes exposing (alignTop, spacing, spread, paddingRight, paddingLeft, yScrollbar, maxHeight, px)
import Element.Events exposing (onClick)
import Element.Input as Input exposing (checkbox)


-- View -------


viewTableMeta : TableMeta -> Element Styles Styles Msg
viewTableMeta meta =
    let
        completeSelection =
            meta.variables
                |> List.all hasSelection

        (buttonStyle, submitButtonAttributes) =
            if completeSelection then
                (Main, [ onClick Submit])
            else
                (Disabled, [])
        
    in
        column Table
            columnAttributes
            [ row None
                titleAttributes
                [ titleElement meta.title
                , (row None
                    [ spacing 5 ]
                    [ button buttonStyle (buttonHeight :: submitButtonAttributes)  <| text "Submit"
                    , button Main [ onClick ToggleTableMetaView, buttonHeight ] <| text "X"
                    ]
                  )
                ]
            , viewVariablesMeta meta.variables
            ]


hasSelection : VariableMeta -> Bool
hasSelection var =
    var.values
        |> List.any .selected


viewVariablesMeta : List VariableMeta -> Element Styles variation Msg
viewVariablesMeta variables =
    column None columnAttributes <|
        List.map viewVariableMeta variables


viewVariableMeta : VariableMeta -> Element Styles variation Msg
viewVariableMeta variable =
    let
        values =
            if variable.sorted then
                List.sortBy .text variable.values
            else
                variable.values
    in
        row None
            [alignTop]
            [ el VariableName [ paddingRight 10 ] <| text variable.text
            , column VariableData
                ([ yScrollbar, maxHeight (px 150) ] ++ listAttributes)
                (values
                    |> List.map (viewValueMeta variable)
                )
            , checkbox None [ paddingLeft 10 ]
                { onChange = \_-> (ToggleSort variable)
                , label = text "sort"
                , checked = variable.sorted
                , options = []
                }
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


toggleVariableSort : VariableMeta -> TableMeta -> TableMeta
toggleVariableSort variable table =
    let
        variables =
            table.variables
                |> mapIf (\var -> var.code == variable.code) toggleSort
    in
        { table | variables = variables }


toggleSort : VariableMeta -> VariableMeta
toggleSort variable =
    { variable | sorted = not variable.sorted }
