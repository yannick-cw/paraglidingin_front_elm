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
import Data.String.Conversions
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

type MyApi = Raw

server :: Server MyApi
server = serveDirectory "./var/www"

userAPI :: Proxy MyApi
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

startServer :: IO ()
startServer = run 8081 app1