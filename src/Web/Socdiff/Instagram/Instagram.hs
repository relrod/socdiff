module Web.Socdiff.Instagram.Instagram where

import qualified Data.Text as T
import Haxl.Core
import Web.Socdiff.Instagram.DataSource

-- | Get a list of followers for the given username
getUsernames :: T.Text -> GenHaxl u [(T.Text, T.Text)]
getUsernames u = dataFetch (GetFollowerUsernames u)
