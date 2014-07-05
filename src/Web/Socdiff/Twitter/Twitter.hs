module Web.Socdiff.Twitter.Twitter where

import qualified Data.Text as T
import Haxl.Core
import Web.Socdiff.Twitter.DataSource

-- | Fetch a list of follower *IDs* for the given username
getFollowers :: T.Text -> GenHaxl u [Integer]
getFollowers u = dataFetch (GetFollowerIDs u)

-- | Get a list of usernames for the given list of user IDs
getUsernames :: [Integer] -> GenHaxl u [T.Text]
getUsernames u = dataFetch (GetUsernames u)
