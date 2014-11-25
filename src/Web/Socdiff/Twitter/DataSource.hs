{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Socdiff.Twitter.DataSource where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.IO.Class
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.Hashable
import Data.List (intercalate, sort)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import qualified Network.HTTP.Client as HTTPClient
import Network.Wreq

import Haxl.Core

data TwitterReq a where
   GetFollowerIDs :: T.Text -> TwitterReq [Integer]
   GetUsernames :: [Integer] -> TwitterReq [T.Text]
  deriving Typeable

deriving instance Eq (TwitterReq a)
deriving instance Show (TwitterReq a)

instance Show1 TwitterReq where show1 = show

instance Hashable (TwitterReq a) where
  hashWithSalt s (GetFollowerIDs username) = hashWithSalt s (1::Int, username)
  hashWithSalt s (GetUsernames uid) = hashWithSalt s (2::Int, uid)

instance StateKey TwitterReq where
  data State TwitterReq = TwitterState { numThreads :: Int
                                       , consumerKey :: T.Text
                                       , consumerSecret :: T.Text }

instance DataSourceName TwitterReq where
  dataSourceName _ = "Twitter"

instance DataSource u TwitterReq where
  fetch = twitterFetch

initGlobalState :: Int -> T.Text -> T.Text -> IO (State TwitterReq)
initGlobalState threads cKey cSecret  = return $ TwitterState threads cKey cSecret

twitterFetch :: State TwitterReq -> Flags -> u -> [BlockedFetch TwitterReq] -> PerformFetch
twitterFetch TwitterState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync consumerKey consumerSecret sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync :: T.Text -> T.Text -> QSem -> BlockedFetch TwitterReq -> IO (Async ())
fetchAsync consumerKey consumerSecret sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $ liftIO $
           getToken consumerKey consumerSecret >>= fetchReq req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

getToken :: T.Text -> T.Text -> IO T.Text
getToken consumerKey consumerSecret = do
  let bearerOpts = defaults & auth ?~ basicAuth (TE.encodeUtf8 consumerKey) (TE.encodeUtf8 consumerSecret)
  bearerResp <- postWith bearerOpts "https://api.twitter.com/oauth2/token" ["grant_type" := C8.pack "client_credentials"]
  return $ bearerResp ^?! responseBody . key "access_token" . _String

fetchReq :: TwitterReq a -> T.Text -> IO a
fetchReq (GetFollowerIDs u) bearerToken = do
  let oAuthOpts = defaults & auth ?~ oauth2Bearer (TE.encodeUtf8 bearerToken)
  resp <- getWith oAuthOpts ("https://api.twitter.com/1.1/followers/ids.json?screen_name=" ++ T.unpack u)
  return $ sort $ resp ^.. responseBody . key "ids" . values . _Integer

fetchReq (GetUsernames []) _ = return mempty
fetchReq (GetUsernames uids) bearerToken = do
  let oAuthOpts = defaults & auth ?~ oauth2Bearer (TE.encodeUtf8 bearerToken)
      uids' = intercalate "," $ fmap show uids
  resp <- Control.Exception.try $
            getWith oAuthOpts ("https://api.twitter.com/1.1/users/lookup.json?user_id=" ++ uids')
            :: IO (Either HTTPClient.HttpException (Response CL8.ByteString))
  return $ case resp of
    Right x -> sort $ x ^.. responseBody . values . key "screen_name" . _String
    Left _ -> mempty
