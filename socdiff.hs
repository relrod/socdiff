module Main where

import Data.List ((\\), intercalate, sort)
import Haxl.Core
import qualified Web.Socdiff.Github.DataSource as Github
import qualified Web.Socdiff.Github.Github as Github
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  githubState <- Github.initGlobalState 10
  ghEnv <- initEnv (stateSet githubState stateEmpty) ()
  putStrLn "Fetching Github followers"
  r <- runHaxl ghEnv $ do
    followers <- Github.getFollowers "CodeBlock" -- TODO: We'll need a config file for this
    return $ sort $ fmap ("Github:CodeBlock:"++) followers -- TODO: Can we use access the data source's name?
  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"

  doesCacheExist <- doesFileExist cachePath
  if doesCacheExist
    then do
      oldCache <- fmap lines (readFile cachePath)
      mapM_ putStrLn (removals oldCache r)
      mapM_ putStrLn (additions oldCache r)
    else do
      putStrLn "No previous run detected. Can't generate a diff."

  writeFile cachePath $ intercalate "\n" r
  appendFile cachePath "\n"
  putStrLn $ "Stored " ++ cachePath

removals :: [String] -> [String] -> [String]
removals old new = fmap ("- "++) (old \\ new)

additions :: [String] -> [String] -> [String]
additions old new = fmap ("+ "++) (new \\ old)
