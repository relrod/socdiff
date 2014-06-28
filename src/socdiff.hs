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
