module Generated.ParaApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


import Generated.Models exposing(..)

decodeTags : Decoder Tags
decodeTags =
    decode Tags
        |> required "tags" (list string)

decodeSaved : Decoder Saved
decodeSaved =
    decode Saved
        |> required "saved" string

decodeSearchResults : Decoder SearchResults
decodeSearchResults =
    decode SearchResults
        |> required "searchRequest" decodeSearchRequest
        |> required "results" (list decodeSearchResult)

decodeSearchRequest : Decoder SearchRequest
decodeSearchRequest =
    decode SearchRequest
        |> required "tag" string

decodeSearchResult : Decoder SearchResult
decodeSearchResult =
    decode SearchResult
        |> required "title" string
        |> required "imgSrc" string
        |> required "href" string
        |> required "description" string

encodeTags : Tags -> Json.Encode.Value
encodeTags x =
    Json.Encode.object
        [ ( "tags", (Json.Encode.list << List.map Json.Encode.string) x.tags )
        ]

postSearch : Tags -> Http.Request (List (SearchResults))
postSearch body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8081"
                , "search"
                ]
        , body =
            Http.jsonBody (encodeTags body)
        , expect =
            Http.expectJson (list decodeSearchResults)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getEmailByEMAIL : String -> Http.Request (Tags)
getEmailByEMAIL capture_EMAIL =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8081"
                , "email"
                , capture_EMAIL |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeTags
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSaveByEmail : String -> Tags -> Http.Request (Saved)
postSaveByEmail capture_email body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8081"
                , "save"
                , capture_email |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeTags body)
        , expect =
            Http.expectJson decodeSaved
        , timeout =
            Nothing
        , withCredentials =
            False
        }