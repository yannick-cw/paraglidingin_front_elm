module Main exposing (..)

import Html
import View.Page exposing (view)
import Update.Update exposing (update)
import Model.Model exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
