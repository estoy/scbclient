module Types exposing (..)

import Http


type alias Model =
    { siteContext : SiteCtx
    , levelContexts : List LevelCtx
    , tableMeta : Maybe TableMeta
    , table : Maybe TableData
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
    }


type alias VariableMetaDTO =
    { code : String
    , text : String
    , values : List String
    , valueTexts : List String
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


type alias Data =
    { key : List String
    , values : List String
    }


type alias Column =
    { code : String
    , text : String
    , type_ : String
    }
