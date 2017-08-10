{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Api where

import           Data.Aeson.Types
import           GHC.Generics
import           Prelude          ()
import           Prelude.Compat
import           Servant
import           Servant.Elm      (ElmType)

newtype Tags = Tags
  { tags :: [String]
  } deriving (Generic)

instance ToJSON Tags

instance ElmType Tags

instance FromJSON Tags

data SearchResults = SearchResults
  { searchRequest :: SearchRequest
  , results       :: [SearchResult]
  } deriving (Generic)

newtype SearchRequest = SearchRequest
  { tag :: String
  } deriving (Generic)

instance FromJSON SearchRequest

instance ToJSON SearchRequest

instance ElmType SearchRequest

instance ToJSON SearchResults

instance ElmType SearchResults

instance FromJSON SearchResults

data SearchResult = SearchResult
  { title       :: String
  , imgSrc      :: String
  , href        :: String
  , description :: String
  } deriving (Generic)

instance FromJSON SearchResult

instance ToJSON SearchResult

instance ElmType SearchResult

newtype Saved = Saved
  { saved :: String
  } deriving (Generic)

instance ToJSON Saved

instance ElmType Saved

type ParaApi
   = "search" :> ReqBody '[ JSON] Tags :> Post '[ JSON] [SearchResults] :<|> "email" :> Capture "EMAIL" String :> Get '[ JSON] Tags :<|> "save" :> Capture "email" String :> ReqBody '[ JSON] Tags :> Post '[ JSON] Saved

type WebServiceApi = ParaApi :<|> Raw

wsApi :: Proxy WebServiceApi
wsApi = Proxy
