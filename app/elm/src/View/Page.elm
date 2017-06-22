module View.Page exposing (view)

import Model.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import View.TagList
import View.SearchResultList
import Helper.Helpers exposing (isValidEmail, styles)
import Html.Attributes exposing (..)
import Css


view : Model -> Html Msg
view model =
    div []
        [ mainHeader
        , div [ class "container" ]
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
        ]


mainHeader : Html msg
mainHeader =
    let
        green =
            Css.rgb 30 104 30

        white =
            Css.rgb 255 255 255
    in
        header
            [ class "content-header"
            , styles
                [ Css.textAlign Css.center
                , Css.backgroundColor green
                , Css.color white
                , Css.height (Css.px 50)
                , Css.marginTop (Css.px 10)
                ]
            ]
            [ text "Paragliding Scraper" ]
