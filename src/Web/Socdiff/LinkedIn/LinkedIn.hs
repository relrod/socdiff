module Web.Socdiff.LinkedIn.LinkedIn where

import qualified Data.Text as T
import Haxl.Core
import Web.Socdiff.LinkedIn.DataSource

-- | Get a list of connection names for the given username.
getConnections :: T.Text -> GenHaxl u [T.Text]
getConnections u = dataFetch (GetConnections u)
