module Web.Socdiff.Facebook.Facebook where

import Control.Applicative
import qualified Data.Text as T
import Facebook
import Haxl.Core
import Web.Socdiff.Facebook.DataSource

-- | Fetch a list of friends for the given user id
getFriends :: T.Text -> GenHaxl u [Friend]
getFriends u = dataFetch (GetFriends (Id u))

getNames :: [Friend] -> [T.Text]
getNames x = friendName <$> x
