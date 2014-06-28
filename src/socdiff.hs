module Main where

import Data.List ((\\), intercalate, sort)
import Haxl.Core
import qualified Web.Socdiff.Github.DataSource as Github
import qualified Web.Socdiff.Github.Github as Github
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"
  createDirectoryIfMissing False cachePath
  github cachePath "Github" "CodeBlock"

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
-- the diff. We can do this nicely and concurrently via Haxl, but this means
-- that our Haxl code is what becomes responsible for updating the cache file.
-- Alternatively, and perhaps better, we could maybe play with returning
-- @([String], [String], [String])@ from Haxl, where the first is a full new
-- list of follower IDs for us to write out to the cache, the second is a list
-- of new followers, and the third is a list of ex followers. The advantage of
-- this is that we can keep all the ugly file writing I/O together here, and
-- still have Haxl do the hard work because concurrency is actually useful here.
-- The disadvantage is that it might be hard to get Haxl to give us that triple.
twitter :: String -> String -> String -> IO ()
twitter = error "TODO"
