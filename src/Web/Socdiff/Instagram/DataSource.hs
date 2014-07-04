{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Socdiff.Instagram.DataSource where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.IO.Class
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.Hashable
import qualified Data.Text as T
import Data.Typeable
import Network.Wreq

import Haxl.Core

data InstagramReq a where
   GetFollowerUsernames :: T.Text -> InstagramReq [(T.Text, T.Text)]
  deriving Typeable

deriving instance Eq (InstagramReq a)
deriving instance Show (InstagramReq a)

instance Show1 InstagramReq where show1 = show

instance Hashable (InstagramReq a) where
  hashWithSalt s (GetFollowerUsernames username) = hashWithSalt s (1::Int, username)

instance StateKey InstagramReq where
  data State InstagramReq = InstagramState { numThreads :: Int
                                           , accessToken :: T.Text
                                           , username :: T.Text }

instance DataSourceName InstagramReq where
  dataSourceName _ = "Instagram"

instance DataSource u InstagramReq where
  fetch = twitterFetch

initGlobalState :: Int -> T.Text -> T.Text -> IO (State InstagramReq)
initGlobalState threads cKey cSecret  = return $ InstagramState threads cKey cSecret

twitterFetch :: State InstagramReq -> Flags -> u -> [BlockedFetch InstagramReq] -> PerformFetch
twitterFetch InstagramState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync accessToken sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync :: T.Text -> QSem -> BlockedFetch InstagramReq -> IO (Async ())
fetchAsync token sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $ liftIO $ fetchReq req token
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

-- Yes, the API really does give UIDs as strings.
getUID :: T.Text -> T.Text -> IO T.Text
getUID token u = do
  resp <- get ("https://api.instagram.com/v1/users/search/?access_token=" ++ T.unpack token ++ "&q=" ++ T.unpack u)
  return $ resp ^. responseBody . key "data" . values . key "id" . _String

followersToTupleList :: Response CL8.ByteString -> [(T.Text, T.Text)]
followersToTupleList resp =
  resp ^.. responseBody . key "data" . _Array . traverse .
    to (\o -> ( o ^?! key "username" . _String
              , o ^?! key "full_name" . _String
              ))


-- If you have >2900 followers, this will probably fail with some weird HTTP
-- code. TODO: Fix that.
getAllFollowers ::
  [(T.Text, T.Text)] -- ^ The result of the previous fetch(es)
  -> Maybe T.Text -- ^ The next cursor URL
  -> IO [(T.Text, T.Text)]
getAllFollowers xs Nothing = return xs
getAllFollowers xs (Just url) = do
  resp <- get (T.unpack url)
  let nextUrl = resp ^? responseBody . key "pagination" . key "next_url" . _String
      followers = followersToTupleList resp
  getAllFollowers (xs ++ followers) nextUrl

fetchReq :: InstagramReq a -> T.Text -> IO a
fetchReq (GetFollowerUsernames user) token = do
  -- The instagram API is even more annoying than Twitter regarding pagination.
  -- At least with Twitter, you can get it to dump you a list of follower IDs
  -- to get out of having to worry about pagination unless you have >5k
  -- followers. With Instagram, you only get 100 follower objects, and they all
  -- contain everything about the user (first name, last name, bio, etc).
  -- There's no way to only get UIDs and then request more information as you
  -- need it (e.g. to render a socdiff diff). Oh, and they limit you to 30 API
  -- requests per hour, so if you ever get more than (29 * 100 = 2900)
  -- followers, socdiff becomes practically useless. (We subtract 1 from 30 due
  -- to the extra call we have to make to get your UID from the API). Alas, we
  -- pretend that this is useful in any way and continue. You've been warned.
  uid <- getUID token user
  resp <- get ("https://api.instagram.com/v1/users/" ++ T.unpack uid ++ "/followed-by?access_token=" ++ T.unpack token)
  let url = resp ^? responseBody . key "pagination" . key "next_url" . _String
      followers = followersToTupleList resp
  getAllFollowers followers url
