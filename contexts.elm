module Contexts exposing (elementFromSite, columnFromLevelContext, modelWithSite, modelWithLevel)

import Types exposing (..)
import Styles exposing (..)
import Attributes exposing (columnAttributes)


-- External ----

import Element exposing (text, el, column, paragraph, Element)
import Element.Events exposing (onClick)


elementFromSite : Site -> Site -> Element Styles variation Msg
elementFromSite selected site =
    let
        style =
            if site == selected then
                Selected
            else
                Deselected
    in
        el style [ onClick <| SelectSite site ] (text site.language)


columnFromLevelContext : LevelCtx -> Element Styles variation Msg
columnFromLevelContext context =
    let
        style =
            if List.any (\level -> level.type_ == "t") context.levels then
                Table
            else
                Main
    in
        column style
            columnAttributes
            (List.map (elementFromLevel context.selected context.index) context.levels)


elementFromLevel : Maybe Level -> Int -> Level -> Element Styles variation Msg
elementFromLevel selected index level =
    let
        style =
            case selected of
                Just sel ->
                    if sel == level then
                        Selected
                    else
                        Deselected

                Nothing ->
                    Deselected
    in
        el style [ onClick <| SelectLevel level index ] (paragraph None [] [text level.text])


modelWithSite : Model -> Site -> List Level -> Model
modelWithSite model site levels =
    let
        oldSiteContext =
            model.siteContext

        oldLevelContexts =
            model.levelContexts
    in
        { model
            | siteContext = { oldSiteContext | selected = site }
            , levelContexts =
                if List.length levels > 0 then
                    [ { index = 0
                      , selected = Nothing
                      , levels = levels
                      }
                    ]
                else
                    []
            , tableMeta = Nothing
        }


modelWithLevel : Model -> Level -> Int -> List Level -> Model
modelWithLevel model level index levels =
    let
        parentContexts =
            List.take index model.levelContexts

        newContext =
            { index = index + 1
            , selected = Nothing
            , levels = levels
            }

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
            | levelContexts = parentContexts ++ [ updatedContext, newContext ]
            , tableMeta = Nothing
        }
