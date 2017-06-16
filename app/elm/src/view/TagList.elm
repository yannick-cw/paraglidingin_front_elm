module View.TagList exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Html.Events exposing (onClick)
import Css exposing (
        flexStart
        , displayFlex
        , alignItems
        )

styles =
    Css.asPairs >> Html.Attributes.style

view : Model -> Html Msg
view model =
    ul [] (model.tags |> List.map tagToLi)


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

