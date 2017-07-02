{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Api
import           Elm         (Spec (Spec), specsToDir, toElmDecoderSource,
                              toElmEncoderSource, toElmTypeSource)
import           Servant.Elm (ElmOptions (..), ElmType, Proxy (Proxy),
                              UrlPrefix (Static), defElmImports, defElmOptions,
                              generateElmForAPIWith)

myElmOpts :: ElmOptions
myElmOpts = defElmOptions {urlPrefix = Static "http://localhost:8081"}

spec :: Spec
spec =
  Spec
    ["Generated", "ParaApi"]
    (defElmImports :
     "import Generated.Models exposing(..)" :
     toElmDecoderSource (Proxy :: Proxy Tags) :
     toElmDecoderSource (Proxy :: Proxy Saved) :
     toElmDecoderSource (Proxy :: Proxy SearchResults) :
     toElmDecoderSource (Proxy :: Proxy SearchResult) :
     toElmEncoderSource (Proxy :: Proxy Tags) : generateElmForAPIWith myElmOpts (Proxy :: Proxy ParaApi))

modelSpec :: Spec
modelSpec =
  Spec
    ["Generated", "Models"]
    [ toElmTypeSource (Proxy :: Proxy Tags)
    , toElmTypeSource (Proxy :: Proxy SearchResults)
    , toElmTypeSource (Proxy :: Proxy SearchResult)
    , toElmTypeSource (Proxy :: Proxy Saved)
    ]

main :: IO ()
main = specsToDir [spec, modelSpec] "elm/src"
