module TableMeta exposing (viewTableMeta, toggleValueForTable, modelWithTableMeta, toggleVariableSort, selectAll, clearAll)

import Types exposing (..)
import Styles exposing (..)
import Utils exposing (mapIf)
import Attributes exposing (columnAttributes, listAttributes, buttonHeight)
import Elements exposing (buttonElement, titleRow)
import Translations exposing (translate)


-- External ------

import Element exposing (Element, text, button, el, column, row, paragraph)
import Element.Attributes exposing (alignTop, spacing, spread, paddingRight, paddingLeft, yScrollbar, maxHeight, px)
import Element.Events exposing (onClick)
import Element.Input as Input exposing (checkbox)


-- View -------


viewTableMeta : TableMeta -> String -> Element Styles Styles Msg
viewTableMeta meta language =
    let
        completeSelection =
            meta.variables
                |> List.all hasSelection
    in
        column Table
            columnAttributes
            [ titleRow meta.title
                [ buttonElement (translate SubmitKey language) Submit completeSelection
                , buttonElement (translate ClearSelectionsKey language) ClearSelection True
                , buttonElement "X" ToggleTableMetaView True
                ]
            , viewVariablesMeta meta.variables language
            ]


hasSelection : VariableMeta -> Bool
hasSelection var =
    var.values
        |> List.any .selected


viewVariablesMeta : List VariableMeta -> String -> Element Styles variation Msg
viewVariablesMeta variables language =
    column None columnAttributes <|
        List.map (viewVariableMeta language) variables


viewVariableMeta : String -> VariableMeta -> Element Styles variation Msg
viewVariableMeta language variable =
    let
        values =
            if variable.sorted then
                List.sortBy .text variable.values
            else
                variable.values
    in
        row None
            [ alignTop, spacing 10 ]
            [ el VariableName [] <| text variable.text
            , column VariableData
                ([ yScrollbar, maxHeight (px 150) ] ++ listAttributes)
                (values
                    |> List.map (viewValueMeta variable)
                )
            , if not variable.time then
                checkbox None
                    []
                    { onChange = \_ -> (ToggleSort variable)
                    , label = text (translate SortKey language)
                    , checked = variable.sorted
                    , options = []
                    }
              else
                buttonElement (translate SelectAllKey language) (SelectAll variable) True
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
        el style [ onClick (ToggleValue var val) ] <| paragraph None [] [ text val.text ]



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


selectAll : VariableMeta -> TableMeta -> TableMeta
selectAll variable table =
    let
        variables =
            table.variables
                |> mapIf (\var -> var.code == variable.code) selectAllValues
    in
        { table | variables = variables }


selectAllValues : VariableMeta -> VariableMeta
selectAllValues variable =
    { variable | values = List.map selectValue variable.values }


selectValue : ValueMeta -> ValueMeta
selectValue val =
    { val | selected = True }


clearAll : TableMeta -> TableMeta
clearAll table =
    let
        variables =
            table.variables
                |> List.map clearSelections
    in
        { table | variables = variables }


clearSelections : VariableMeta -> VariableMeta
clearSelections variable =
    { variable | values = List.map deselectValue variable.values }


deselectValue : ValueMeta -> ValueMeta
deselectValue val =
    { val | selected = False }
