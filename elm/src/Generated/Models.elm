module Generated.Models exposing (..)

type alias Tags =
    { tags : List (String)
    }

type alias SearchResults =
    { results : List (SearchResult)
    }

type alias SearchResult =
    { header : String
    , text : String
    , img : String
    }

type alias Saved =
    { saved : String
    }