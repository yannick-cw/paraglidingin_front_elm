{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Prelude ()
import Prelude.Compat
import Network.Wai.Handler.Warp
import Servant
import Api
import ClientConnections

server :: Server ParaApi
server = searchForTags
  :<|> tagsForEmail
  :<|> saveTagsForEmail
  :<|>  serveDirectory "./var/www"

startServer :: IO ()
startServer = run 8081 $ serve paraApi server