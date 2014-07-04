{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import qualified Data.Configurator as Cfg
import Data.List ((\\), intercalate, sort)
import qualified Data.Text as T
import Haxl.Core
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

import qualified Web.Socdiff.Github.DataSource as Github
import qualified Web.Socdiff.Github.Github as Github

import qualified Web.Socdiff.Instagram.DataSource as Instagram
import qualified Web.Socdiff.Instagram.Instagram as Instagram

import qualified Web.Socdiff.Twitter.DataSource as Twitter
import qualified Web.Socdiff.Twitter.Twitter as Twitter

data Followers =
  GithubResult {
      ghList   :: [T.Text]
    , username :: String
    }
  | TwitterResult {
      twList   :: [Integer]
    , username :: String
    }
  | InstagramResult {
      instagramList :: [T.Text]
    , username      :: String
    }

main :: IO ()
main = do
  config <- Cfg.load [Cfg.Required "socdiff.cfg"]

  instagramToken <- Cfg.require config "instagram.access_token" :: IO T.Text
  instagramUser <- Cfg.require config "instagram.username" :: IO T.Text

  twitterKey <- Cfg.require config "twitter.key" :: IO T.Text
  twitterSecret <- Cfg.require config "twitter.secret" :: IO T.Text

  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"
  createDirectoryIfMissing False cachePath

  -- Step one: Initialize the data store's state (give it login creds, etc)
  twitterState    <- Twitter.initGlobalState 2 twitterKey twitterSecret
  instagramState  <- Instagram.initGlobalState 2 instagramToken
  githubState     <- Github.initGlobalState 2

  -- Step two: Add it to the StateStore so that we can actually use it
  let st =
        stateSet githubState .
        stateSet instagramState .
        stateSet twitterState $
        stateEmpty

  env' <- initEnv st ()

  -- Step three: Perform the actual data fetching (concurrently)
  (twitterFollowers, githubFollowers, instagramFollowers) <-
    runHaxl env' $ (,,) <$>
      twitter' "relrod6" <*>
      github' "CodeBlock" <*>
      instagram' (T.unpack instagramUser)

  handleResults cachePath env' [twitterFollowers, githubFollowers, instagramFollowers]

generateDiff :: String -> String -> [String] -> [String] -> IO ()
generateDiff source cachePath added removed = do
  doesCacheExist <- doesFileExist cachePath
  if doesCacheExist
    then do
      mapM_ putStrLn $ fmap (("- " ++ source ++ ":") ++) removed
      mapM_ putStrLn $ fmap (("+ " ++ source ++ ":") ++) added
    else
      putStrLn "No previous run detected. Can't generate a diff."

github' :: T.Text -> GenHaxl u Followers
github' user = do
  githubFollowers <- sort <$> Github.getFollowers user
  return $ GithubResult githubFollowers (T.unpack user)

instagram' :: String -> GenHaxl u Followers
instagram' user = do
  instagramFollowers <- Instagram.getFollowers (T.pack user)
  return $ InstagramResult (sort (fst <$> instagramFollowers)) user

twitter' :: String -> GenHaxl u Followers
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
    process (GithubResult xs user) = do
      let filename' = filename "Github" user
      createIfMissing filename'
      oldCache <- fmap lines (readFile filename')
      generateDiff "Github" filename' (additions oldCache $ T.unpack <$> xs) (removals oldCache $ T.unpack <$> xs)
      writeViaText filename' xs

    process (InstagramResult xs user) = do
      let filename' = filename "Instagram" user
      createIfMissing filename'
      oldCache <- fmap lines (readFile filename')
      generateDiff "Instagram" filename' (additions oldCache $ T.unpack <$> xs) (removals oldCache $ T.unpack <$> xs)
      writeViaText filename' xs

    process (TwitterResult xs user) = do
      let filename' = filename "Twitter" user
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
