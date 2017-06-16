module View exposing (view)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import View.TagList
import View.SearchResultList
import Helper.Helpers exposing (isValidEmail)

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
            , View.TagList.view model.tags
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
            , View.SearchResultList.view model.searchResults
            ]
        ]
