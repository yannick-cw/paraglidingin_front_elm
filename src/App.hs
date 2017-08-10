{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           Api
import           ClientConnections
import           Network.Wai.Handler.Warp
import           Prelude                  ()
import           Prelude.Compat
import           Servant
import           System.IO.Error

paraServer :: Server ParaApi
paraServer = searchForTags :<|> fetchTagsForEmail :<|> saveTagsForEmail

server :: Server WebServiceApi
server = paraServer :<|> serveDirectory "./var/www"

startServer :: IO ()
startServer =
  catchIOError
    (do _ <- print "running on 8081"
        run 8081 $ serve wsApi server)
    (print . (++) "failed startup with: " . show)
