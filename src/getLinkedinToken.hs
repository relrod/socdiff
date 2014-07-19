{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Char8 as C8
import qualified Data.Configurator as Cfg
import Data.Aeson.Lens
import Data.List (intercalate)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO
import Network.Wreq
import System.IO

redirectUrl :: T.Text
redirectUrl = "https://codeblock.github.io/socdiff/linkedin.html&state=f00bar12"

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
  config         <- Cfg.load [Cfg.Required "socdiff.cfg"]
  liClientId     <- Cfg.require config "linkedin.id" :: IO T.Text
  liClientSecret <- Cfg.require config "linkedin.secret" :: IO T.Text

  hSetBuffering stdout NoBuffering
  putStrLn $ "Please go to this url in a *browser*, and paste the resulting " ++
           "code below:"
  putStrLn $ "https://www.linkedin.com/uas/oauth2/authorization?response_" ++
             "type=code&client_id=" ++ T.unpack liClientId ++ "&redirect_uri" ++
             "=" ++ T.unpack redirectUrl
  putStr "Code: "
  code <- Data.Text.IO.getLine
  token <- getToken code redirectUrl liClientId liClientSecret

  putStrLn "Add this to the linkedin {...} section of your socdiff config"
  putStrLn "(or replace any existing such line):"
  putStrLn $ "  token = \"" ++ T.unpack token ++ "\""

  return ()

getToken :: T.Text -> T.Text -> T.Text -> T.Text -> IO T.Text
getToken code redirect clientId clientSecret = do
  bearerResp <- post
                ("https://www.linkedin.com/uas/oauth2/accessToken?" ++ data')
                ["" := C8.pack ""]
  return $ bearerResp ^?! responseBody . key "access_token" . _String
  where
    data' =
      intercalate "&" $ (\(x, y) -> T.unpack x <> "=" <> T.unpack y) <$>
      [ ("code"         , code)
      , ("redirect_uri" , redirect)
      , ("client_id"    , clientId)
      , ("client_secret", clientSecret)
      , ("grant_type"   , "authorization_code")
      ]
