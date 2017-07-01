module Request.Tags exposing (..)

import Json.Decode exposing (field)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Model.Model exposing (..)


tagsForEmail : String -> Cmd Msg
tagsForEmail email =
    let
        url =
            "/email/" ++ email

        request =
            Http.get url decodeTags
    in
        Http.send FetchedTags request


saveTags : List Tag -> String -> Cmd Msg
saveTags tags email =
    let
        url =
            "/save/" ++ email

        body =
            (encodeTags tags) |> Http.jsonBody

        request =
            Http.post url body Decode.string
    in
        Http.send SavedEmail request


decodeTags : Decode.Decoder (List Tag)
decodeTags =
    Decode.at [ "tags" ] (Decode.list Decode.string)

encodeTags : List Tag -> Encode.Value
encodeTags tags =
    Encode.object [ ( "tags", Encode.list (tags |> List.map Encode.string) ) ]
