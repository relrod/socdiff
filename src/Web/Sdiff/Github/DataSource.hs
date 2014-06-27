{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Sdiff.Github.DataSource where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.IO.Class
import Data.Hashable
import Data.Typeable
import Github.Users.Followers

import Haxl.Core

data GithubReq a where
   GetFollowers :: String -> GithubReq [String]
  deriving Typeable

deriving instance Eq (GithubReq a)
deriving instance Show (GithubReq a)

instance Show1 GithubReq where show1 = show

instance Hashable (GithubReq a) where
  hashWithSalt s (GetFollowers username) = hashWithSalt s (1::Int, username)

instance StateKey GithubReq where
  data State GithubReq = GithubState { numThreads :: Int }

instance DataSourceName GithubReq where
  dataSourceName _ = "Github"

instance DataSource u GithubReq where
  fetch = githubFetch

initGlobalState :: Int -> IO (State GithubReq)
initGlobalState threads = return $ GithubState threads

githubFetch :: State GithubReq -> Flags -> u -> [BlockedFetch GithubReq] -> PerformFetch
githubFetch GithubState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync :: QSem -> BlockedFetch GithubReq -> IO (Async ())
fetchAsync sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $ liftIO $ fetchReq req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

fetchReq :: GithubReq a -> IO a
fetchReq (GetFollowers u) = do
  Right followers <- usersFollowing u
  return $ map githubOwnerLogin followers
