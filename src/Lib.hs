{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib
 (startServer
 )where

import Prelude ()
import Prelude.Compat

import Data.Aeson.Types
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type Tag = String

newtype Tags = Tags
  { tags :: [Tag] } deriving Generic

instance ToJSON Tags
instance FromJSON Tags

newtype SearchResults = SearchResults
  { results :: [SearchResult] } deriving Generic

instance ToJSON SearchResults

data SearchResult = SearchResult
    { header :: String
    , text :: String
    , img :: String
    } deriving Generic

instance ToJSON SearchResult

defaultSearchResults :: SearchResults
defaultSearchResults = SearchResults [SearchResult "header" "text" "img.com"]

defaultTags :: Tags
defaultTags = Tags ["tag1", "tag2"]

type MyApi = "search" :> ReqBody '[JSON] Tags :> Post '[JSON] SearchResults
  :<|> "email" :> Capture "email" String :> Get '[JSON] Tags
  :<|> "save" :> Capture "email" String :> ReqBody '[JSON] Tags :> Post '[PlainText] String
  :<|> Raw

searchForTags :: Tags -> Handler SearchResults
searchForTags _ = return defaultSearchResults

tagsForEmail :: String -> Handler Tags
tagsForEmail _ = return defaultTags

saveTagsForEmail :: String -> Tags -> Handler String
saveTagsForEmail _ _ = return "saved"

server :: Server MyApi
server = searchForTags
  :<|> tagsForEmail
  :<|> saveTagsForEmail
  :<|>  serveDirectory "./var/www"

userAPI :: Proxy MyApi
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

startServer :: IO ()
startServer = run 8081 app1