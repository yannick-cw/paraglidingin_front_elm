module View.TagInput exposing (tagInput)

import Html exposing (..)
import Css
import Model.Model exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (..)
import Helper.Helpers exposing (styles)

tagInput : String -> Html Msg
tagInput inputTag =
    div [ styles [ Css.displayFlex, Css.flexDirection Css.row ] ]
      [ input
          [ type_ "text"
          , placeholder "tag"
          , onInput InputTagState
          , value inputTag
          , class "form-control input-lg"
          , styles [ Css.width (Css.px 300) ]
          ]
          []
      , button
          [ onClick (AddTag inputTag)
          , disabled (inputTag |> String.isEmpty)
          , styles [ Css.width (Css.px 38) ]
          ]
          [ text "+" ]
      ]
