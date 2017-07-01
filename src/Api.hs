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

newtype Tags = Tags
  { tags :: [String]
  } deriving (Generic)

instance ToJSON Tags

instance FromJSON Tags

newtype SearchResults = SearchResults
  { results :: [SearchResult]
  } deriving (Generic)

instance ToJSON SearchResults

data SearchResult = SearchResult
  { header :: String
  , text   :: String
  , img    :: String
  } deriving (Generic)

instance ToJSON SearchResult

type ParaApi
   = "search" :> ReqBody '[ JSON] Tags :> Post '[ JSON] SearchResults :<|> "email" :> Capture "EMAIL" String :> Get '[ JSON] Tags :<|> "save" :> Capture "email" String :> ReqBody '[ JSON] Tags :> Post '[ PlainText] String :<|> Raw

paraApi :: Proxy ParaApi
paraApi = Proxy
