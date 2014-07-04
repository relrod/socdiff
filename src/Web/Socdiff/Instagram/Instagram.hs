module Web.Socdiff.Instagram.Instagram where

import qualified Data.Text as T
import Haxl.Core
import Web.Socdiff.Instagram.DataSource

-- | Get a list of followers for the given username
getFollowers :: T.Text -> GenHaxl u [(T.Text, T.Text)]
getFollowers u = dataFetch (GetFollowerUsernames u)
