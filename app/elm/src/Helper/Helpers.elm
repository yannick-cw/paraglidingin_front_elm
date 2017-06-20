module Helper.Helpers exposing (..)

import List exposing (..)
import Regex exposing (regex, contains)


distinct : List a -> List a
distinct list =
    (list |> (List.foldl dropIfExists [])) |> reverse


dropIfExists : a -> List a -> List a
dropIfExists a acc =
    if (acc |> member a) then
        acc
    else
        a :: acc


isValidEmail : String -> Bool
isValidEmail email =
    contains (regex ".+@.+") email
