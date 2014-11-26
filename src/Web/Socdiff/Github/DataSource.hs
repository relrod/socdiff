{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Socdiff.Github.DataSource where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.IO.Class
import Data.Hashable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Github.Auth
import Github.Data.Definitions
--import Github.Repos.Forks
import Github.Repos
import Github.Repos.Starring
import Github.Repos.Watching
import Github.Users.Followers

import Prelude hiding (mapM, mapM_)
import Haxl.Core
import Haxl.Prelude

data GithubReq a where
   GetFollowers :: T.Text -> GithubReq [T.Text]
   GetRepos :: T.Text -> GithubReq [T.Text]
   GetStargazers :: T.Text -> T.Text -> GithubReq [T.Text]
   GetWatchers :: T.Text -> T.Text -> GithubReq [T.Text]
  deriving Typeable

deriving instance Eq (GithubReq a)
deriving instance Show (GithubReq a)

instance Show1 GithubReq where show1 = show

instance Hashable (GithubReq a) where
  hashWithSalt s (GetFollowers username)       = hashWithSalt s (1::Int, username)
  hashWithSalt s (GetRepos username)           = hashWithSalt s (2::Int, "repos" <> username)
  hashWithSalt s (GetStargazers username repo) = hashWithSalt s (3::Int, "stargazers" <> username <> repo)
  hashWithSalt s (GetWatchers username repo)   = hashWithSalt s (4::Int, "watchers" <> username <> repo)

instance StateKey GithubReq where
  data State GithubReq = GithubState { numThreads :: Int, ghauth :: GithubAuth }

instance DataSourceName GithubReq where
  dataSourceName _ = "Github"

instance DataSource u GithubReq where
  fetch = githubFetch

initGlobalState :: Int -> T.Text -> IO (State GithubReq)
initGlobalState threads token = return $ GithubState threads (GithubBasicAuth (T.encodeUtf8 token) "x-oauth-basic")

githubFetch :: State GithubReq -> Flags -> u -> [BlockedFetch GithubReq] -> PerformFetch
githubFetch GithubState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync ghauth sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync :: GithubAuth -> QSem -> BlockedFetch GithubReq -> IO (Async ())
fetchAsync ghauth sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $ liftIO $ fetchReq req ghauth
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

fetchReq :: GithubReq a -> GithubAuth -> IO a
fetchReq (GetFollowers u) _ = do
  Right followers <- usersFollowing (T.unpack u)
  return $ T.pack . githubOwnerLogin <$> followers
fetchReq (GetRepos u) ghauth = do
  Right r <- userRepos' (Just ghauth) (T.unpack u) Github.Repos.All
  return $ map (T.pack . repoName) r
fetchReq (GetStargazers u r) ghauth = do
  s <- stargazersFor (Just ghauth) (T.unpack u) (T.unpack r)
  case s of
   Right s' -> return $ map (T.pack . githubOwnerLogin) s'
   Left _   -> return []
fetchReq (GetWatchers u r) ghauth = do
  s <- watchersFor' (Just ghauth) (T.unpack u) (T.unpack r)
  case s of
   Right s' -> return $ map (T.pack . githubOwnerLogin) s'
   Left _   -> return []
