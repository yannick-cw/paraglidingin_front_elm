module Routing exposing (..)

import Model.Model exposing (..)
import Navigation exposing (Location)
import UrlParser exposing (..)

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map Home (s "tags")
        ]

parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
