module Request.Search exposing (search)

import Json.Decode exposing (field)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Model.Model exposing (..)
import Generated.ParaApi exposing (postSearch)
import Generated.Models exposing (Tags)


search : Tags -> Cmd Msg
search tags =
    Http.send FetchedSearchResults (postSearch tags)
