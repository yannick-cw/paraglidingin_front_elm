{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
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

server :: Server ParaApi
server = searchForTags :<|> fetchTagsForEmail :<|> saveTagsForEmail :<|> serveDirectory "./var/www"

startServer :: IO ()
startServer = run 8081 $ serve paraApi server
--startServer = clientExecute
