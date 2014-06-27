module Web.Socdiff.Github.Github where

import Web.Socdiff.Github.DataSource

import Haxl.Core

-- | Fetch a list of followers for the given username
getFollowers :: String -> GenHaxl u [String]
getFollowers u = dataFetch (GetFollowers u)
