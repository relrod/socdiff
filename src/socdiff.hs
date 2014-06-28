module Main where

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

data TwitterCreds = TwitterCreds { consumerKey :: String
                                 , consumerSecret :: String
                                 }

main :: IO ()
main = do
  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"
  createDirectoryIfMissing False cachePath
  followers <- getAllFollowersHackyDontRelyOnThis (TwitterCreds "KEY" "SECRET") "relrod6" "CodeBlock"
  handleResults cachePath followers

generateDiff :: String -> String -> [String] -> IO ()
generateDiff source cachePath r = do
  doesCacheExist <- doesFileExist cachePath
  if doesCacheExist
    then do
      oldCache <- fmap lines (readFile cachePath)
      mapM_ putStrLn $ fmap (("- " ++ source ++ ":") ++) (removals oldCache r)
      mapM_ putStrLn $ fmap (("+ " ++ source ++ ":") ++) (additions oldCache r)
    else do
      putStrLn "No previous run detected. Can't generate a diff."
  where
    removals = (\\)
    additions = flip (\\)

-- | Get all followers concurrently! (In a very hacky way - don't rely on this.)
-- We can probably split this into per-network functions like we had originally
-- and just pass around both the 'Haxl.Core.StateStore', and the
-- 'Haxl.Core.Monad.GenHaxl', modifying each of them as we go, then at the very
-- end, we can call runHaxl and pass it the description we've built up.
-- It will be much better than this, in any case.
-- This is a hack, don't rely on it.
getAllFollowersHackyDontRelyOnThis
                :: TwitterCreds
                -> String -- ^ Twitter username
                -> String -- ^ Github username
                -> IO [Followers]
getAllFollowersHackyDontRelyOnThis (TwitterCreds cKey cSecret) tw gh = do
  githubState <- Github.initGlobalState 2
  twitterState <- Twitter.initGlobalState 2 (T.pack cKey) (T.pack cSecret)
  let st =
        stateSet twitterState .
        stateSet githubState $
        stateEmpty
  env' <- initEnv st ()

  --putStrLn $ "Fetching " ++ source ++ " followers"
  followers <- runHaxl env' $ do
    githubFollowers <- fmap sort $ Github.getFollowers gh
    twitterFollowers <- fmap sort $ Twitter.getFollowers tw
    return $ [
        GithubResult githubFollowers gh
      , TwitterResult twitterFollowers tw
      ]

  return followers

-- | Handle the resulting data fetches once they are all completed.
handleResults :: String -> [Followers] -> IO ()
handleResults cachePath fs = mapM_ process fs
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
