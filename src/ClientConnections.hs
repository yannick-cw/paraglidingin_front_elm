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

search :: String -> ClientM SearchResults
search searchString = client api (SearchRequest searchString)

newtype SearchRequest = SearchRequest
  { tag :: String
  } deriving (Generic)

instance ToJSON SearchRequest

fetchSearch :: Tags -> Handler [SearchResults]
fetchSearch t = traverse (ExceptT . fmap (either errTo Right) . clientExecute) (tags t)

errTo :: ServantError -> Either ServantErr SearchResults
errTo _ = Right $ SearchResults "" []

clientExecute :: String -> IO (Either ServantError SearchResults)
clientExecute searchString = do
  manager <- newManager tlsManagerSettings
  runClientM (search searchString) (ClientEnv manager (BaseUrl Http "localhost" 8082 ""))
