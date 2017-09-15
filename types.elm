module Types exposing (..)

import Http


type alias Model =
    { siteContext : SiteCtx
    , levelContexts : List LevelCtx
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
