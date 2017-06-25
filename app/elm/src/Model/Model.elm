module Model.Model exposing (..)

import Http

init : ( Model, Cmd Msg )
init =
    ( Model [] "" "" [], Cmd.none )

type alias Model =
    { tags : List Tag
    , inputTag : Tag
    , email : String
    , searchResults : List SearchResult
    }


type alias SearchResult =
    { header : String
    , text : String
    , img : String
    }


type alias Tag =
    String


type Msg
    = InputTagState String
    | AddTag String
    | RemoveTag String
    | EmailChange String
    | FetchedTags (Result Http.Error (List Tag))
    | FetchedSearchResults (Result Http.Error (List SearchResult))
    | SavedEmail (Result Http.Error String)
    | Save (List Tag) String

