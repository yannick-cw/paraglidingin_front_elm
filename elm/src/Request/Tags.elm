module Request.Tags exposing (..)

import Json.Decode exposing (field)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Model.Model exposing (..)
import Generated.ParaApi exposing (postSaveByEmail, getEmailByEMAIL)
import Generated.Models exposing (Tags)


tagsForEmail : String -> Cmd Msg
tagsForEmail email =
    Http.send FetchedTags (getEmailByEMAIL email)


saveTags : Tags -> String -> Cmd Msg
saveTags tags email =
    Http.send SavedEmail (postSaveByEmail email tags)
