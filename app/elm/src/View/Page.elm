module View.Page exposing (view)

import Model.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import View.TagList
import View.TagInput exposing (tagInput)
import View.SearchResultList
import View.PageHeader exposing (pageHeader)
import View.EmailInput exposing (emailInput)
import Helper.Helpers exposing (styles)
import Css


view : Model -> Html Msg
view model =
    div []
        [ pageHeader
        , div [ class "container"
         , styles
             [ Css.marginTop (Css.px 20)
             , Css.marginBottom (Css.px 20)
             ]
            ]
            [ div
                [ class "col-md-9"
                , styles [ Css.margin2 (Css.px 0) (Css.auto)]
                ]
                [ emailInput model
                , View.TagList.view model.tags
                , tagInput model.inputTag
                , View.SearchResultList.view model.searchResults
                ]
            ]
        ]
