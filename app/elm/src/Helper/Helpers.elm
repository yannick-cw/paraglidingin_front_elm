module Helper.Helpers exposing (..)

import List exposing (..)


distinct : List a -> List a
distinct list =
    (list |> (List.foldl dropIfExists [])) |> reverse


dropIfExists : a -> List a -> List a
dropIfExists a acc =
    if (acc |> member a) then
        acc
    else
        a :: acc
