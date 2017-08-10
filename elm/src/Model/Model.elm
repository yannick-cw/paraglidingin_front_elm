module Model.Model exposing (..)

import Navigation exposing (Location)
import Http
import Generated.Models exposing (SearchResult, SearchResults, Saved, Tags, SearchRequest)


initialModel : Route -> Model
initialModel route =
    { tags = Tags []
    , inputTag = ""
    , email = ""
    , searchResults = []
    , route = route
    }


type alias Model =
    { tags : Tags
    , inputTag : String
    , email : String
    , searchResults : List SearchResults
    , route : Route
    }


type Route
    = Home
    | NotFoundRoute


type Msg
    = InputTagState String
    | AddTag String
    | RemoveTag String
    | EmailChange String
    | FetchedTags (Result Http.Error Tags)
    | FetchedSearchResults (Result Http.Error (List SearchResults))
    | SavedEmail (Result Http.Error Saved)
    | Save Tags String
    | OnLocationChange Location
