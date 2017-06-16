module View.SearchResultList exposing (view)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    ul [ class "list-group" ] (model.searchResults |> List.map searchResToLi)

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
