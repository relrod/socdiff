{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Socdiff.Facebook.DataSource where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Control.Lens
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson.Lens
import Data.Conduit
import Data.Conduit.List hiding (map, mapM, mapM_)
import Data.Hashable
import Data.Monoid
import qualified Data.Text as T
import Data.Typeable
import Facebook
import Haxl.Core
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit

data FacebookReq a where
   GetFriends :: UserId -> FacebookReq [T.Text]
  deriving Typeable

deriving instance Eq (FacebookReq a)
deriving instance Show (FacebookReq a)

instance Show1 FacebookReq where show1 = show

instance Hashable (FacebookReq a) where
  hashWithSalt s (GetFriends (Id uid)) = hashWithSalt s (1::Int, uid)

instance StateKey FacebookReq where
  data State FacebookReq =
    FacebookState
       { credentials :: Credentials
       , userAccessToken :: UserAccessToken
       , manager :: Manager
       , numThreads :: Int
       }

instance DataSourceName FacebookReq where
  dataSourceName _ = "Facebook"

instance DataSource u FacebookReq where
  fetch = githubFetch

initGlobalState :: Int -> Credentials -> UserAccessToken -> IO (State FacebookReq)
initGlobalState threads creds token = do
  manager <- newManager tlsManagerSettings
  return $ FacebookState creds token manager threads

githubFetch :: State FacebookReq -> Flags -> u -> [BlockedFetch FacebookReq] -> PerformFetch
githubFetch FacebookState{..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    sem <- newQSem numThreads
    asyncs <- mapM (fetchAsync credentials manager userAccessToken sem) bfs
    inner
    mapM_ wait asyncs

fetchAsync ::
  Credentials
  -> Manager
  -> UserAccessToken
  -> QSem
  -> BlockedFetch FacebookReq
  -> IO (Async ())
fetchAsync creds manager tok sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $
           runResourceT $ runFacebookT creds manager $ fetchReq tok req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

fetchReq
  :: UserAccessToken
  -> FacebookReq a
  -> FacebookT Auth (ResourceT IO) a
fetchReq tok (GetFriends (Id uid)) = do
  f <- getObject ("/" <> uid <> "/taggable_friends") [] (Just tok)
  source <- fetchAllNextPages f
  resp <- source $$ consume :: FacebookT Auth (ResourceT IO) [Value]
  return $ map (\x -> x ^. key "name" . _String) resp
