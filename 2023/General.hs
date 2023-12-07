module General
  ( preciseTimeIt, retrieveInput
  ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           System.TimeIt          (timeItT)
import           Text.Printf            (printf)

import           Data.List.Split  (splitOn)
import           Network.Curl     (CurlOption (CurlCookie, CurlProxy),
                                   curlGetString)
import           System.Directory (getHomeDirectory)

cookiePath = "/adventOfCode/cookies.json"

adventURL = "https://adventofcode.com/"

proxyPath = "/adventOfCode/proxy"

preciseTimeIt :: (MonadIO m, Show a) => Int -> m a -> m a
preciseTimeIt prec ioa = do
  (t, a) <- timeItT ioa
  liftIO $ printf ("CPU time" ++ ":%6." ++ show prec ++ "fs\n") t
  return a

parseCookie :: String -> IO String
parseCookie path = do
  rawCookie <- readFile path
  let cookieLength = length rawCookie
  let (name:value:_) =
        map (\(_:y:_) -> init y) .
        filter (\(x:_) -> x == "name" || x == "value") .
        map (splitOn ":") .
        splitOn "\n" .
        filter (\x -> x /= ' ' && x /= '\"') . drop 4 . take (cookieLength - 2) $
        rawCookie
  return $ name ++ "=" ++ value

retrieveInput :: Int -> Int -> [String] -> IO String
retrieveInput year day args = do
  case args of
    [] -> remoteInput year day
    a  -> readFile $ "day" ++ show day ++ "-test.txt"

remoteInput :: Int -> Int -> IO String
remoteInput year day = do
  let url = adventURL ++ show year ++ "/day/" ++ show day ++ "/input"
  home <- getHomeDirectory
  cookie <- parseCookie $ home ++ cookiePath
  proxy <- readFile (home ++ proxyPath)
--  (code, rsp) <- curlGetString url [CurlProxy (init proxy), CurlCookie cookie]
  (code, rsp) <- curlGetString url [CurlCookie cookie]
  return rsp