{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.Configurator as Cfg
import qualified Data.Text as T
import qualified Facebook as Facebook
import qualified Network.HTTP.Conduit as H
import System.IO

-- | Facebook's API is annoying too (I am sensing a pattern here: oAuth is
-- annoying). The workflow works like this (and is only valid for 60 days):
--
-- 1) Create an application, put the name/id/secret in the config file (you
--    only have to do this once)
-- 2) Run `socdiff-facebook-token`, and update `facebook.token` in `socdiff.cfg`
--
-- Internally, we construct a URL that you must visit in a browser, then you get
-- redirected to a static page (hosted on github in the gh-pages branch of
-- socdiff), that gives you a "code" to paste back into this program.
--
-- Once you paste this code, you'll be given an access token, which we exchange
-- for an "extended" access token, and you won't have to do this again for 60
-- days. Sigh.
main :: IO ()
main = do
  config <- Cfg.load [Cfg.Required "socdiff.cfg"]
  fbName   <- Cfg.require config "facebook.app_name" :: IO T.Text
  fbId     <- Cfg.require config "facebook.id" :: IO T.Text
  fbSecret <- Cfg.require config "facebook.secret" :: IO T.Text

  let creds = Facebook.Credentials fbName fbId fbSecret

  url <- H.withManager $ \m -> Facebook.runFacebookT creds m (Facebook.getUserAccessTokenStep1
                                                              "https://codeblock.github.io/socdiff/facebook.html"
                                                              ["user_friends", "public_profile"])

  hSetBuffering stdout NoBuffering
  putStrLn "Please go to this url in a *browser*, and paste the resulting code below:"
  putStrLn (T.unpack url)
  putStr "Code: "
  code <- getLine
  token1 <- H.withManager $ \m -> Facebook.runFacebookT creds m (Facebook.getUserAccessTokenStep2
                                                                 "https://codeblock.github.io/socdiff/facebook.html"
                                                                 [("code", C8.pack code)])
  Right (Facebook.UserAccessToken _ d _) <- H.withManager $ \m -> Facebook.runFacebookT creds m
                                                                  (Facebook.extendUserAccessToken token1)
  putStrLn "Add this to the facebook {...} section of your socdiff config"
  putStrLn "(or replace any existing such line):"
  putStrLn $ "  token = \"" ++ T.unpack d ++ "\""
  return ()
