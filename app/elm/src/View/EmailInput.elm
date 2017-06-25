module View.EmailInput exposing (emailInput)

import Html exposing (..)
import Css
import Model.Model exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (..)
import Helper.Helpers exposing (styles, isValidEmail)

emailInput : Model -> Html Msg
emailInput model =
    div [ styles [ Css.displayFlex , Css.flexDirection Css.row, Css.marginBottom (Css.px 20) ] ]
        [ input
            [ type_ "email"
            , onInput EmailChange
            , placeholder "email"
            , class "form-control input-lg"
            ]
            []
        , button
            [ onClick (Save model.tags model.email)
            , disabled (not (isValidEmail model.email))
            , styles [ Css.width (Css.pct 20) ]
            ]
            [ text "subscribe tags" ]
        ]
