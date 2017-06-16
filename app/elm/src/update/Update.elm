module Update exposing (update)

import Json.Decode exposing (field)
import Json.Decode as Decode
import Json.Encode as Encode

import Model exposing (..)
import Helper.Helpers exposing (distinct, isValidEmail)
import Regex exposing (regex, contains)
import Http


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTagState tag ->
            ( { model | inputTag = tag }, Cmd.none )

        AddTag tag ->
            let
                newTags =
                    ([ tag ] |> List.append model.tags)
                        |> distinct
            in
                ( { model | tags = newTags, inputTag = "" }, search newTags )

        RemoveTag tag ->
            let
                newTags =
                   List.filter ((/=) tag) model.tags
            in
                ( { model | tags = newTags }, search newTags )

        EmailChange email ->
            ( { model | email = email }
            , if (isValidEmail email) then
                tagsForEmail email
              else
                Cmd.none
            )

        FetchedTags (Ok tags) ->
            let
                newTags =
                    (tags |> List.append model.tags) |> distinct
            in
                ( { model | tags = newTags }, search newTags )

        FetchedTags (Err er) ->
            Debug.log (toString er)
                ( model, Cmd.none )

        Save tags email ->
            ( model, saveTags tags email )

        SavedEmail (Ok msg) ->
            Debug.log msg
                ( model, Cmd.none )

        SavedEmail (Err er) ->
            Debug.log (toString er)
                ( model, Cmd.none )

        FetchedSearchResults (Ok searchResults) ->
            ( { model | searchResults = searchResults }, Cmd.none )

        FetchedSearchResults (Err er) ->
            Debug.log (toString er)
                ( model, Cmd.none )


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
            "/save"

        body =
            (encodeTagsAndEmail tags email) |> Http.jsonBody

        request =
            Http.post url body Decode.string
    in
        Http.send SavedEmail request


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


decodeTags : Decode.Decoder (List Tag)
decodeTags =
    Decode.at [ "tags" ] (Decode.list Decode.string)


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


encodeTagsAndEmail : List Tag -> String -> Encode.Value
encodeTagsAndEmail tags email =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "tags", encodeTags tags )
        ]


encodeTags : List Tag -> Encode.Value
encodeTags tags =
    Encode.object [ ( "tags", Encode.list (tags |> List.map Encode.string) ) ]
