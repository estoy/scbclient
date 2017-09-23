module Contexts exposing (..)

import Types exposing (..)
import Styles exposing (..)
import Attributes exposing (..)
import Element exposing (..)
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
        el style [ onClick <| SelectLevel level index ] (text level.text)
