module View.SearchResultList exposing (view)

import Model.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Helper.Helpers exposing (styles)
import Css
    exposing
        ( flexDirection
        , flexStart
        , displayFlex
        , alignItems
        , column
        )


view : List SearchResult -> Html Msg
view searchResults =
    div []
        [
          h1 [] [ text "Offers" ]
        , ul [ class "list-group" ] (searchResults |> List.map searchResToLi)
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
                [ h4 [] [ text res.title ]
                , span [] [ text res.description ]
                ]
            , img [ src res.imgSrc, width 200 ] []
            ]
        ]
