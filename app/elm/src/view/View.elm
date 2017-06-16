module View exposing (view)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (regex, contains)
import View.TagList
import Css
    exposing
        ( flexDirection
        , flexStart
        , displayFlex
        , alignItems
        , column
        )

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
            , View.TagList.view model
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

-- TODO currently duplicate
isValidEmail : String -> Bool
isValidEmail email =
    contains (regex ".+@.+") email
