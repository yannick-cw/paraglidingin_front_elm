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

tagsForEmail :: String -> Handler Tags
tagsForEmail _ = return $ Tags ["tag1", "tag2"]

saveTagsForEmail :: String -> Tags -> Handler Saved
saveTagsForEmail _ _ = return $ Saved "ok"

type ParaSearchApi = "search" :> ReqBody '[ JSON] SearchRequest :> Post '[ JSON] SearchResults

api :: Proxy ParaSearchApi
api = Proxy

search :: SearchRequest -> ClientM SearchResults
search = client api

fetchSearch :: Tags -> Handler [SearchResults]
fetchSearch t = traverse (ExceptT . clientExecute . SearchRequest) (tags t)

errTo :: SearchRequest -> ServantError -> Either ServantErr SearchResults
errTo request _ = Right $ SearchResults request []

clientExecute :: SearchRequest -> IO (Either ServantErr SearchResults)
clientExecute request = do
  manager <- newManager tlsManagerSettings
  searchResOrError <- runClientM (search request) (ClientEnv manager (BaseUrl Http "localhost" 8082 ""))
  return (either (errTo request) Right searchResOrError)
