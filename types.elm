module Types exposing (..)

import Styles exposing (Styles)


-- External -----

import Http
import Element exposing (Element)


type Msg
    = SelectSite Site
    | SiteLoaded Site (Result Http.Error (List Level))
    | SelectLevel Level Int
    | LevelLoaded Level Int (Result Http.Error (List Level))
    | TableMetaLoaded Level Int (Result Http.Error TableMeta)
    | ToggleTableMetaView
    | ToggleTableDataView
    | ToggleValue VariableMeta ValueMeta
    | Submit
    | TableLoaded (Result Http.Error TableData)
    | ToggleSort VariableMeta
    | TogglePlot
    | SelectAll VariableMeta
    | ClearSelection


type alias Model =
    { siteContext : SiteCtx
    , levelContexts : List LevelCtx
    , tableMeta : Maybe TableMeta
    , table : Maybe TableData
    , showPlot : Bool
    , latestError : Maybe Http.Error
    }


type alias SiteCtx =
    { selected : Site
    , sites : List Site
    }


type alias LevelCtx =
    { index : Int
    , selected : Maybe Level
    , levels : List Level
    }


type alias Site =
    { language : String
    , url : Url
    }


type alias Level =
    { id : String
    , type_ : String
    , text : String
    }


type alias Url =
    String


type alias TableMeta =
    { title : String
    , variables : List VariableMeta
    }


type alias VariableMeta =
    { code : String
    , text : String
    , values : List ValueMeta
    , time : Bool
    , sorted : Bool
    }


type alias VariableMetaDTO =
    { code : String
    , text : String
    , values : List String
    , valueTexts : List String
    , time : Bool
    }


type alias ValueMeta =
    { value : String
    , text : String
    , selected : Bool
    }


type alias TableData =
    { data : List Data
    , columns : List Column
    }


type alias DataDTO =
    { key : List String
    , values : List String
    }


type alias Data =
    { key : List String
    , time : String
    , values : List String
    }


type alias DataSequence =
    { key : List String
    , points : List DataPoint
    }


type alias DataPoint =
    { time : String
    , values : List String
    }


type alias Column =
    { code : String
    , text : String
    , type_ : String
    }


type alias Button variation =
    Element Styles variation Msg


type TranslationKey
    = PlotKey
    | SortKey
    | SelectAllKey
    | SubmitKey
    | ClearSelectionsKey
