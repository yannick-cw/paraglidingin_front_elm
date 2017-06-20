module Request.Search exposing (search)

import Json.Decode exposing (field)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Request.Tags exposing (encodeTags)
import Model.Model exposing (..)


search : List Tag -> Cmd Msg
search tags =
    let
        url =
            "/search"

        body =
            (encodeTags tags) |> Http.jsonBody

        request =
            Http.post url body decodeSearchResults
    in
        Http.send FetchedSearchResults request




decodeSearchResults : Decode.Decoder (List SearchResult)
decodeSearchResults =
    Decode.at [ "results" ] (Decode.list decodesSearchResult)


decodesSearchResult : Decode.Decoder SearchResult
decodesSearchResult =
    Decode.map3
        SearchResult
        (Decode.at [ "header" ] Decode.string)
        (Decode.at [ "text" ] Decode.string)
        (Decode.at [ "img" ] Decode.string)
