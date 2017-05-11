module Main exposing (..)

import Html exposing (program)
import Json.Decode exposing (field)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (regex, contains)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tags : List Tag
    , inputTag : Tag
    , email : String
    , searchResults : List String
    }


type alias Tag =
    String


type Msg
    = InputTagState String
    | AddTag String
    | RemoveTag String
    | EmailChange String
    | FetchedTags (Result Http.Error (List Tag))
    | FetchedSearchResults (Result Http.Error (List String))
    | SavedEmail (Result Http.Error String)
    | Save (List Tag) String


init : ( Model, Cmd Msg )
init =
    ( Model [ "tag1", "tag2" ] "" "" [], Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTagState tag ->
            ( { model | inputTag = tag }, Cmd.none )

        AddTag tag ->
            let
                newTags =
                    tag :: model.tags
            in
                ( { model | tags = newTags, inputTag = "" }, search newTags )

        RemoveTag tag ->
            let
                newTags =
                    model.tags |> (List.filter ((/=) tag))
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
                    model.tags |> List.append tags
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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "email"
            , onInput EmailChange
            , placeholder "email"
            ]
            []
        , ul [] (model.tags |> List.map tagToLi)
        , input
            [ type_ "text"
            , placeholder "tag"
            , onInput InputTagState
            , value model.inputTag
            ]
            []
        , button
            [ onClick (AddTag model.inputTag)
            , disabled (model.inputTag |> String.isEmpty)
            ]
            [ text "+" ]
        , button
            [ onClick (Save model.tags model.email)
            , disabled (not (isValidEmail model.email))
            ]
            [ text "save" ]
        , ul [] (model.searchResults |> List.map (\res -> (li [] [ span [] [ text res ] ])))
        ]


isValidEmail : String -> Bool
isValidEmail email =
    contains (regex ".+@.+") email


tagToLi : Tag -> Html Msg
tagToLi tag =
    li []
        [ button [ onClick (RemoveTag tag) ] [ text "-" ]
        , span [] [ text tag ]
        ]


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


decodeSearchResults : Decode.Decoder (List String)
decodeSearchResults =
    Decode.at [ "results" ] (Decode.list Decode.string)


encodeTagsAndEmail : List Tag -> String -> Encode.Value
encodeTagsAndEmail tags email =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "tags", encodeTags tags )
        ]


encodeTags : List Tag -> Encode.Value
encodeTags tags =
    Encode.object [ ( "tags", Encode.list (tags |> List.map Encode.string) ) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
