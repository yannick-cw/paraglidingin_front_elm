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

server :: Server ParaApi
server = searchForTags :<|> fetchTagsForEmail :<|> saveTagsForEmail :<|> serveDirectory "./var/www"

startServer :: IO ()
startServer =
  catchIOError
    (do _ <- print "running on 8081"
        run 8081 $ serve paraApi server)
    (print . (++) "failed startup with: " . show)
