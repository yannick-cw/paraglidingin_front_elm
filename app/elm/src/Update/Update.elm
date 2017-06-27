module Update.Update exposing (update)

import Json.Decode exposing (field)
import Json.Decode as Decode
import Json.Encode as Encode
import Routing exposing (parseLocation)
import Model.Model exposing (..)
import Request.Tags exposing (..)
import Request.Search exposing (search)
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

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )
