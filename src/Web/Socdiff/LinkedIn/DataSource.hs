{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Socdiff.LinkedIn.DataSource where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.IO.Class
import Control.Lens
import Data.Aeson.Lens
import Data.Hashable
import qualified Data.Text as T
import Data.Typeable
import Network.Wreq

import Haxl.Core

data LinkedInReq a where
   GetConnections :: T.Text -> LinkedInReq [T.Text]
  deriving Typeable

deriving instance Eq (LinkedInReq a)
deriving instance Show (LinkedInReq a)

instance Show1 LinkedInReq where show1 = show

instance Hashable (LinkedInReq a) where
  hashWithSalt s (GetConnections username) = hashWithSalt s (1::Int, username)

instance StateKey LinkedInReq where
  data State LinkedInReq = LinkedInState { numThreads :: Int
                                         , accessToken :: T.Text
                                         }

instance DataSourceName LinkedInReq where
  dataSourceName _ = "LinkedIn"

instance DataSource u LinkedInReq where
  fetch = instagramFetch

initGlobalState :: Int -> T.Text -> IO (State LinkedInReq)
initGlobalState threads token  = return $ LinkedInState threads token

instagramFetch ::
  State LinkedInReq ->
  Flags ->
  u ->
  [BlockedFetch LinkedInReq] ->
  PerformFetch
instagramFetch LinkedInState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync accessToken sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync :: T.Text -> QSem -> BlockedFetch LinkedInReq -> IO (Async ())
fetchAsync token sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $ liftIO $ fetchReq req token
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

-- TODO: Pagination.
fetchReq :: LinkedInReq a -> T.Text -> IO a
fetchReq (GetConnections user) token = do
  resp <- get $ "https://api.linkedin.com/v1/people/" ++ T.unpack user ++
                "/connections:(formatted-name)?format=json&" ++
                "oauth2_access_token=" ++ T.unpack token
  let people = resp ^.. responseBody . key "values" . values . key "formattedName" . _String
  return people
