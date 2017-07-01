module ClientConnections where

import Api
import Servant.Server (Handler)

searchForTags :: Tags -> Handler SearchResults
searchForTags _ = return $ SearchResults [SearchResult "header" "text" "img.com"]

tagsForEmail :: String -> Handler Tags
tagsForEmail _ = return $ Tags ["tag1", "tag2"]

saveTagsForEmail :: String -> Tags -> Handler String
saveTagsForEmail _ _ = return "saved"
