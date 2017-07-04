module Model.Model exposing (..)

import Navigation exposing (Location)
import Http
import Generated.Models exposing (SearchResult, SearchResults, Saved, Tags)


initialModel : Route -> Model
initialModel route =
    { tags = Tags []
    , inputTag = ""
    , email = ""
    , searchResults = SearchResults []
    , route = route
    }


type alias Model =
    { tags : Tags
    , inputTag : String
    , email : String
    , searchResults : SearchResults
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
    | FetchedSearchResults (Result Http.Error SearchResults)
    | SavedEmail (Result Http.Error Saved)
    | Save Tags String
    | OnLocationChange Location
