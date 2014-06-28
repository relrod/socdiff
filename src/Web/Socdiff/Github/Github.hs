module Web.Socdiff.Github.Github where

import Haxl.Core
import Web.Socdiff.Github.DataSource

-- | Fetch a list of followers for the given username
getFollowers :: String -> GenHaxl u [String]
getFollowers u = dataFetch (GetFollowers u)
