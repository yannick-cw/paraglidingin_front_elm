module View.SearchResultList exposing (view)

import Model.Model exposing (..)
import Generated.Models exposing (SearchResults, SearchResult)
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


view : List SearchResults -> Html Msg
view searchResults =
    div []
        ([ h1 [] [ text "Offers" ] ]
            ++ List.map (multipleSearchResToLists) searchResults
        )


multipleSearchResToLists : SearchResults -> Html Msg
multipleSearchResToLists searchResults =
    div []
        [ h2 [] [ text searchResults.searchRequest.tag ]
        , ul [ class "list-group" ] (searchResults.results |> List.map searchResToLi)
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
