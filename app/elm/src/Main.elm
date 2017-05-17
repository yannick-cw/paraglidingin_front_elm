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
import Css
    exposing
        ( flexDirection
        , flexStart
        , displayFlex
        , alignItems
        , column
        )
import Helper.Helpers exposing (distinct)


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
                    ([ tag ] |> List.append model.tags)
                        |> distinct
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



-- VIEW


styles =
    Css.asPairs >> Html.Attributes.style


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "col-md-9" ]
            [ input
                [ type_ "email"
                , onInput EmailChange
                , placeholder "email"
                , class "form-control input-lg"
                ]
                []
            , ul [] (model.tags |> List.map tagToLi)
            , input
                [ type_ "text"
                , placeholder "tag"
                , onInput InputTagState
                , value model.inputTag
                , class "form-control input-lg"
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
            , ul [ class "list-group" ] (model.searchResults |> List.map searchResToLi)
            ]
        ]


isValidEmail : String -> Bool
isValidEmail email =
    contains (regex ".+@.+") email


searchResToLi : SearchResult -> Html Msg
searchResToLi res =
    li
        [ class "list-group-item" ]
        [ div
            [ styles
                [ displayFlex
                , alignItems flexStart
                ]
            ]
            [ div
                [ styles
                    [ displayFlex
                    , flexDirection column
                    , alignItems flexStart
                    ]
                ]
                [ span [] [ text res.header ]
                , span [] [ text res.text ]
                ]
            , img [ src res.img, width 200 ] []
            ]
        ]


tagToLi : Tag -> Html Msg
tagToLi tag =
    li
        [ styles
            [ displayFlex
            , alignItems flexStart
            ]
        ]
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
