{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.List ((\\), intercalate, sort)
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
  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"
  createDirectoryIfMissing False cachePath

  -- Step one: Initialize the data store's state (give it login creds, etc)
  twitterState <- Twitter.initGlobalState 2 "Ve9l0xYXsNM8pIPd92hhA" "LO5rxmTzJeMA3UvRD0NTSdeKS2shZK23LcNWSBlBo" -- TODO: Config file
  githubState  <- Github.initGlobalState 2

  -- Step two: Add it to the StateStore so that we can actually use it
  let st =
        stateSet twitterState .
        stateSet githubState $
        stateEmpty

  env' <- initEnv st ()

  -- Step three: Perform the actual data fetching (concurrently)
  allFollowers <- runHaxl env' $
                    (\x y -> [x, y]) <$>
                    twitter' "relrod6" <*>
                    github' "CodeBlock"

  handleResults cachePath allFollowers

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
