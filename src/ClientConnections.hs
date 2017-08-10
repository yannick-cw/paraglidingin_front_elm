{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module ClientConnections where

import           Api
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS
import           Servant.API
import           Servant.Client
import           Servant.Server             (Handler, ServantErr)

searchForTags :: Tags -> Handler SearchResults
searchForTags _ = return $ SearchResults [SearchResult "header" "text" "img.com"]

tagsForEmail :: String -> Handler Tags
tagsForEmail _ = return $ Tags ["tag1", "tag2"]

saveTagsForEmail :: String -> Tags -> Handler Saved
saveTagsForEmail _ _ = return $ Saved "ok"

type SearchAPI = "xxx" :> Capture "query" String :> Get '[ JSON] SearchRes

api :: Proxy SearchAPI
api = Proxy

search :: String -> ClientM SearchRes
search = client api

newtype SearchRes = SearchRes
  { query :: String
  } deriving (Generic, Show)

instance FromJSON SearchRes

queries :: ClientM SearchRes
queries = search "mal"

-- prove of concept of using client library with server
fetchTagsForEmail :: String -> Handler Tags
fetchTagsForEmail _ = ExceptT $ fmap (either errTo rightTo) clientExecute

-- currently request is just prove of concept and gets replaced with tag result
errTo :: ServantError -> Either ServantErr Tags
errTo _ = Right $ Tags []

rightTo :: SearchRes -> Either ServantErr Tags
rightTo _ = Right $ Tags ["tag"]

clientExecute :: IO (Either ServantError SearchRes)
clientExecute = do
  manager <- newManager tlsManagerSettings
  runClientM queries (ClientEnv manager (BaseUrl Http "www.holidaycheck.de" 80 ""))
