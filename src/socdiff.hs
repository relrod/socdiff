{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.Configurator as Cfg
import Data.List ((\\), intercalate, sort)
import qualified Data.Text as T
import Haxl.Core
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

import qualified Web.Socdiff.Github.DataSource as Github
import qualified Web.Socdiff.Github.Github as Github

import qualified Web.Socdiff.Twitter.DataSource as Twitter
import qualified Web.Socdiff.Twitter.Twitter as Twitter

data Followers =
  GithubResult {
      ghList   :: [String]
    , username :: String
    }
  | TwitterResult {
      twList   :: [Integer]
    , username :: String
    }

main :: IO ()
main = do
  config <- Cfg.load [Cfg.Required "socdiff.cfg"]
  twitterKey <- (Cfg.require config "twitter.key") :: IO T.Text
  twitterSecret <- (Cfg.require config "twitter.secret") :: IO T.Text

  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"
  createDirectoryIfMissing False cachePath

  -- Step one: Initialize the data store's state (give it login creds, etc)
  twitterState <- Twitter.initGlobalState 2 twitterKey twitterSecret
  githubState  <- Github.initGlobalState 2

  -- Step two: Add it to the StateStore so that we can actually use it
  let st =
        stateSet twitterState .
        stateSet githubState $
        stateEmpty

  env' <- initEnv st ()

  -- Step three: Perform the actual data fetching (concurrently)
  (twitterFollowers, githubFollowers) <-
    runHaxl env' $ (,) <$>
      twitter' "relrod6" <*>
      github' "CodeBlock"

  handleResults cachePath [twitterFollowers, githubFollowers]

generateDiff :: String -> String -> [String] -> IO ()
generateDiff source cachePath r = do
  doesCacheExist <- doesFileExist cachePath
  if doesCacheExist
    then do
      oldCache <- fmap lines (readFile cachePath)
      mapM_ putStrLn $ fmap (("- " ++ source ++ ":") ++) (removals oldCache r)
      mapM_ putStrLn $ fmap (("+ " ++ source ++ ":") ++) (additions oldCache r)
    else
      putStrLn "No previous run detected. Can't generate a diff."
  where
    removals = (\\)
    additions = flip (\\)

twitter' :: String -> GenHaxl u Followers
twitter' user = do
  twitterFollowers <- sort <$> Twitter.getFollowers user
  return $ TwitterResult twitterFollowers user

github' :: String -> GenHaxl u Followers
github' user = do
  githubFollowers <- sort <$> Github.getFollowers user
  return $ GithubResult githubFollowers user

-- TODO: This can probably be cleaned up a bit.

-- | Handle the resulting data fetches once they are all completed.
handleResults :: String -> [Followers] -> IO ()
handleResults cachePath = mapM_ process
  where
    filename source user = cachePath </> source ++ "_" ++ user

    process :: Followers -> IO ()
    process (GithubResult xs user) = do
      let filename' = filename "Github" user
      generateDiff "Github" filename' xs
      writeFile filename' $ intercalate "\n" xs
      appendFile filename' "\n"
      putStrLn $ "Stored " ++ filename'

    process (TwitterResult xs user) = do
      let filename' = filename "Twitter" user
          xs' = fmap show xs
      generateDiff "Twitter" filename' xs'
      writeFile filename' $ intercalate "\n" xs'
      appendFile filename' "\n"
      putStrLn $ "Stored " ++ filename'
