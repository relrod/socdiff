module Main where

import Data.List (intercalate)
import Haxl.Core
import qualified Web.Socdiff.Github.DataSource as Github
import qualified Web.Socdiff.Github.Github as Github
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  githubState <- Github.initGlobalState 10
  ghEnv <- initEnv (stateSet githubState stateEmpty) ()
  putStrLn "Fetching Github followers"
  r <- runHaxl ghEnv $ do
    followers <- Github.getFollowers "CodeBlock" -- TODO: We'll need a config file for this
    return $ fmap ("Github:"++) followers -- TODO: Can we use access the data source's name?
  home <- getHomeDirectory
  let cachePath = home </> ".socdiff_cache"
  writeFile cachePath ""
  appendFile cachePath (intercalate "\n" r)
  appendFile cachePath "\n"
  putStrLn $ "Stored " ++ cachePath
