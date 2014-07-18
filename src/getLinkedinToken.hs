{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.Configurator as Cfg
import qualified Data.Text as T
import System.IO

-- | What do you know. LinkedIn uses oAuth too.
--
-- In fact the flow is very similar to, and just as annoying as, Facebook.
--
-- 1) Create an application
--    (https://www.linkedin.com/secure/developer?newapp=)
--
-- 2) Run `socdiff-linkedin-token`, and update `linkedin.token` in `socdiff.cfg`
--
-- Internally, we construct a URL that you must visit in a browser, then you get
-- redirected to a static page (hosted on github in the gh-pages branch of
-- socdiff), that gives you a "code" to paste back into this program.
--
-- Once you paste this code, you'll be given an access token, which lasts for
-- 60 days, then you have to do it again. Ain't it grand? Think about it:
-- People actually thought this was a good idea.
main :: IO ()
main = do
  config     <- Cfg.load [Cfg.Required "socdiff.cfg"]
  liClientId <- Cfg.require config "linkedin.id" :: IO T.Text

  hSetBuffering stdout NoBuffering
  putStrLn "Please go to this url in a *browser*, and paste the resulting code below:"
  putStrLn $ "https://www.linkedin.com/uas/oauth2/authorization?response_type=" ++
             "code&client_id=" ++ T.unpack liClientId ++ "&redirect_uri=" ++
             "https://codeblock.github.io/socdiff/linkedin.html"
  putStr "Code: "
  code <- getLine
  return ()
