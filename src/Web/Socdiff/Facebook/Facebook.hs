module Web.Socdiff.Facebook.Facebook where

import qualified Data.Text as T
import Facebook
import Haxl.Core
import Web.Socdiff.Facebook.DataSource

-- | Fetch a list of friends for the given user id
getFriends :: T.Text -> GenHaxl u [T.Text]
getFriends u = dataFetch (GetFriends (Id u))
