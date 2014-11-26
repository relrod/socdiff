module Web.Socdiff.Github.Github where

import Control.Applicative
import qualified Data.Text as T
import Haxl.Core
import Web.Socdiff.Github.DataSource

-- | Fetch a list of followers for the given username
getFollowers :: T.Text -> GenHaxl u [T.Text]
getFollowers u = dataFetch (GetFollowers u)

-- | Fetch a list of repos for the given username
getRepos :: T.Text -> GenHaxl u [T.Text]
getRepos u = dataFetch (GetRepos u)

-- | Fetch a list of stargazers for the given repository
getStargazers :: T.Text -> T.Text -> GenHaxl u (T.Text, [T.Text])
getStargazers u r = (,) r <$> dataFetch (GetStargazers u r)

-- | Fetch a list of watchers for the given repository
getWatchers :: T.Text -> T.Text -> GenHaxl u (T.Text, [T.Text])
getWatchers u r = (,) r <$> dataFetch (GetWatchers u r)
