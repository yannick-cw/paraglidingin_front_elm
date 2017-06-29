module View.PageHeader exposing (pageHeader)

import Html
import Css exposing (..)
import Html.Attributes as Attributes
import Helper.Helpers exposing (styles)

pageHeader : Html.Html msg
pageHeader =
    let
        green =
            rgb 30 104 30

        white =
            rgb 255 255 255
    in
        Html.header
            [ Attributes.class "content-header"
            , styles
                [ textAlign center
                , backgroundColor green
                , color white
                , height (px 50)
                ]
            ]
            [ Html.text "FP-Paragliding Scraper" ]
