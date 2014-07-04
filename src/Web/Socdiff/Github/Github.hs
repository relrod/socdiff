module Web.Socdiff.Github.Github where

import qualified Data.Text as T
import Haxl.Core
import Web.Socdiff.Github.DataSource

-- | Fetch a list of followers for the given username
getFollowers :: T.Text -> GenHaxl u [T.Text]
getFollowers u = dataFetch (GetFollowers u)
