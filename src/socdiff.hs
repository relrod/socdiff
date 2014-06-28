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

main :: IO ()
main = do
  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"
  createDirectoryIfMissing False cachePath
  github cachePath "Github" "CodeBlock"
  twitter cachePath "Twitter" "relrod6" "CONSUMER_KEY" "CONSUMER_SECRET"

generateDiff :: String -> [String] -> IO ()
generateDiff cachePath r = do
  doesCacheExist <- doesFileExist cachePath
  if doesCacheExist
    then do
      oldCache <- fmap lines (readFile cachePath)
      mapM_ putStrLn $ fmap ("- "++) (removals oldCache r)
      mapM_ putStrLn $ fmap ("+ "++) (additions oldCache r)
    else do
      putStrLn "No previous run detected. Can't generate a diff."
  where
    removals = (\\)
    additions = flip (\\)

github :: String -> String -> String -> IO ()
github cachePath source username = do
  let filename = cachePath </> source ++ "_" ++ username

  githubState <- Github.initGlobalState 10
  ghEnv <- initEnv (stateSet githubState stateEmpty) ()
  putStrLn $ "Fetching " ++ source ++ " followers"

  r <- runHaxl ghEnv $ do
    followers <- Github.getFollowers username
    return $ sort followers

  generateDiff filename r

  writeFile filename $ intercalate "\n" r
  appendFile filename "\n"
  putStrLn $ "Stored " ++ filename

-- Some thoughts about twitter implementation follow.

-- | For twitter, we have to do things a bit differently because their API is
-- annoying. Rather than storing usernames, we store user IDs. We can store up
-- to 5000 of them without needing to worry about pagination (which we currently
-- don't).
--
-- The 5000 user IDs get saved in a similar way to GitHub and our diff is
-- concerned with the user IDs that change. After we know which IDs changed
-- (i.e., were added or removed), we can look up the username and show those in
-- the diff.
twitter ::
  String -- ^ Directory containing the cache files
  -> String -- ^ Name of the source
  -> String -- ^ Username to look up
  -> String -- ^ consumer key (twitter API)
  -> String -- ^ consumer secret (twitter API)
  -> IO ()
twitter cachePath source username cKey cSecret = do
  let filename = cachePath </> source ++ "_" ++ username

  twitterState <- Twitter.initGlobalState 10 (T.pack cKey) (T.pack cSecret)
  twEnv <- initEnv (stateSet twitterState stateEmpty) ()
  putStrLn $ "Fetching " ++ source ++ " followers"

  r <- runHaxl twEnv $ do
    followers <- Twitter.getFollowers username
    return . sort . map show $ followers

  generateDiff filename r

  writeFile filename $ intercalate "\n" r
  appendFile filename "\n"
  putStrLn $ "Stored " ++ filename
