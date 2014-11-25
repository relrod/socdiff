{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import qualified Data.Configurator as Cfg
import Data.List ((\\), intercalate, sort)
import qualified Data.Text as T
import Data.Time.Clock
import Haxl.Core
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

import qualified Facebook
import qualified Web.Socdiff.FB.DataSource as Facebook
import qualified Web.Socdiff.FB.FB as Facebook

import qualified Web.Socdiff.Github.DataSource as Github
import qualified Web.Socdiff.Github.Github as Github

import qualified Web.Socdiff.Instagram.DataSource as Instagram
import qualified Web.Socdiff.Instagram.Instagram as Instagram

import qualified Web.Socdiff.LinkedIn.DataSource as LinkedIn
import qualified Web.Socdiff.LinkedIn.LinkedIn as LinkedIn

import qualified Web.Socdiff.Twitter.DataSource as Twitter
import qualified Web.Socdiff.Twitter.Twitter as Twitter

data Followers =
    FacebookResult {
      fbList   :: [T.Text]
    , username :: T.Text
    }
  | GithubResult {
      ghList   :: [T.Text]
    , username :: T.Text
    }
  | TwitterResult {
      twList   :: [Integer]
    , username :: T.Text
    }
  | InstagramResult {
      instagramList :: [T.Text]
    , username      :: T.Text
    }
  | LinkedInResult {
      linkedInList :: [T.Text]
    , username :: T.Text
    }

main :: IO ()
main = do
  config <- Cfg.load [Cfg.Required "socdiff.cfg"]

  fbUser   <- Cfg.require config "facebook.userid" :: IO T.Text
  fbName   <- Cfg.require config "facebook.app_name" :: IO T.Text
  fbId     <- Cfg.require config "facebook.id" :: IO T.Text
  fbSecret <- Cfg.require config "facebook.secret" :: IO T.Text
  fbToken  <- Cfg.require config "facebook.token" :: IO T.Text

  now <- getCurrentTime
  let fbCreds = Facebook.Credentials fbName fbId fbSecret
      fbUAT   = Facebook.UserAccessToken (Facebook.Id fbUser) fbToken now

  linkedInToken <- Cfg.require config "linkedin.token" :: IO T.Text
  linkedInUser  <- Cfg.require config "linkedin.username" :: IO T.Text

  instagramToken <- Cfg.require config "instagram.access_token" :: IO T.Text
  instagramUser  <- Cfg.require config "instagram.username" :: IO T.Text

  twitterKey    <- Cfg.require config "twitter.key" :: IO T.Text
  twitterSecret <- Cfg.require config "twitter.secret" :: IO T.Text

  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"
  createDirectoryIfMissing False cachePath

  -- Step one: Initialize the data store's state (give it login creds, etc)
  facebookState   <- Facebook.initGlobalState 2 fbCreds fbUAT
  githubState     <- Github.initGlobalState 2
  instagramState  <- Instagram.initGlobalState 2 instagramToken
--  linkedInState   <- LinkedIn.initGlobalState 2 linkedInToken
  twitterState    <- Twitter.initGlobalState 2 twitterKey twitterSecret

  -- Step two: Add it to the StateStore so that we can actually use it
  let st =
        stateSet facebookState .
        stateSet githubState .
        stateSet instagramState .
  --      stateSet linkedInState .
        stateSet twitterState $
        stateEmpty

  env' <- initEnv st ()

  -- Step three: Perform the actual data fetching (concurrently)
  (
      fbFriends
    , twitterFollowers
    , githubFollowers
    , instagramFollowers
--    , linkedInConnections
    ) <-
    runHaxl env' $ (,,,) <$>
      facebook' fbUser <*>
      github' "relrod" <*>
      twitter' "relrod6" <*>
      instagram' instagramUser -- <*>
--      linkedIn' linkedInUser

  handleResults cachePath env' [ fbFriends
                               , githubFollowers
                               , instagramFollowers
--                               , linkedInConnections
                               , twitterFollowers
                               ]

generateDiff :: String -> String -> [String] -> [String] -> IO ()
generateDiff source cachePath added removed = do
  doesCacheExist <- doesFileExist cachePath
  if doesCacheExist
    then do
      mapM_ putStrLn $ fmap (("- " ++ source ++ ":") ++) removed
      mapM_ putStrLn $ fmap (("+ " ++ source ++ ":") ++) added
    else
      putStrLn "No previous run detected. Can't generate a diff."

facebook' :: T.Text -> GenHaxl u Followers
facebook' user = do
  friends <- sort <$> Facebook.getFriends user
  return $ FacebookResult friends user

github' :: T.Text -> GenHaxl u Followers
github' user = do
  githubFollowers <- sort <$> Github.getFollowers user
  return $ GithubResult githubFollowers user

instagram' :: T.Text -> GenHaxl u Followers
instagram' user = do
  instagramFollowers <- Instagram.getFollowers user
  return $ InstagramResult (sort (fst <$> instagramFollowers)) user

linkedIn' :: T.Text -> GenHaxl u Followers
linkedIn' user = do
  linkedInConnections <- LinkedIn.getConnections user
  return $ LinkedInResult (sort linkedInConnections) user

twitter' :: T.Text -> GenHaxl u Followers
twitter' user = do
  twitterFollowers <- sort <$> Twitter.getFollowers user
  return $ TwitterResult twitterFollowers user

-- TODO: This can probably be cleaned up a bit.

-- | Handle the resulting data fetches once they are all completed.
handleResults :: String -> Env u -> [Followers] -> IO ()
handleResults cachePath env' = mapM_ process
  where
    filename source user = cachePath </> source ++ "_" ++ user
    createIfMissing f = do
      doesCacheExist <- doesFileExist f
      unless doesCacheExist $ writeFile f ""

    writeViaText filename' xs =
      do
        writeFile filename' $ intercalate "\n" $ T.unpack <$> xs
        appendFile filename' "\n"
        putStrLn $ "Stored " ++ filename'

    removals = (\\)

    additions = flip (\\)

    process :: Followers -> IO ()
    process (FacebookResult xs user) = do
      let filename' = filename "Facebook" (T.unpack user)
      createIfMissing filename'
      oldCache <- fmap lines (readFile filename')
      generateDiff "Facebook" filename' (additions oldCache $ T.unpack <$> xs) (removals oldCache $ T.unpack <$> xs)
      writeViaText filename' xs

    process (GithubResult xs user) = do
      let filename' = filename "Github" (T.unpack user)
      createIfMissing filename'
      oldCache <- fmap lines (readFile filename')
      generateDiff "Github" filename' (additions oldCache $ T.unpack <$> xs) (removals oldCache $ T.unpack <$> xs)
      writeViaText filename' xs

    process (InstagramResult xs user) = do
      let filename' = filename "Instagram" (T.unpack user)
      createIfMissing filename'
      oldCache <- fmap lines (readFile filename')
      generateDiff "Instagram" filename' (additions oldCache $ T.unpack <$> xs) (removals oldCache $ T.unpack <$> xs)
      writeViaText filename' xs

    process (LinkedInResult xs user) = do
      let filename' = filename "LinkedIn" (T.unpack user)
      createIfMissing filename'
      oldCache <- fmap lines (readFile filename')
      generateDiff "LinkedIn" filename' (additions oldCache $ T.unpack <$> xs) (removals oldCache $ T.unpack <$> xs)
      writeViaText filename' xs

    process (TwitterResult xs user) = do
      let filename' = filename "Twitter" (T.unpack user)
          xs'       = show <$> xs
      createIfMissing filename'
      oldCache <- fmap lines (readFile filename')
      (added, removed) <-
        runHaxl env' $ (,) <$>
          Twitter.getUsernames (read <$> additions oldCache xs') <*>
          Twitter.getUsernames (read <$> removals oldCache xs')
      generateDiff "Twitter" filename' (T.unpack <$> added) (T.unpack <$> removed)
      writeFile filename' $ intercalate "\n" xs'
      appendFile filename' "\n"
      putStrLn $ "Stored " ++ filename'
