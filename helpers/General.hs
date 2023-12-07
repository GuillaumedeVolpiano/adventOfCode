module General
  ( preciseTimeIt
  , customPreciseTimeIt
  , retrieveInput
  ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           System.TimeIt          (timeItT)
import           Text.Printf            (printf)

import           Data.List.Split        (splitOn)
import           Network.Curl           (CurlOption (CurlCookie, CurlProxy),
                                         curlGetString)
import           System.Directory       (getHomeDirectory)

cookiePath = "/adventOfCode/cookies.json"

adventURL = "https://adventofcode.com/"

testPath = "/adventOfCode/test/"

proxyPath = "/adventOfCode/proxy"

preciseTimeIt :: (MonadIO m, Show a) => Int -> m a -> m a
preciseTimeIt = customPreciseTimeIt "CPU time: "

customPreciseTimeIt :: (MonadIO m, Show a) => String -> Int -> m a -> m a
customPreciseTimeIt name prec ioa = do
  (t, a) <- timeItT ioa
  liftIO $ printf (name ++ ":%6." ++ show prec ++ "fs\n") t
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

retrieveInput :: Int -> Int -> Bool -> IO String
retrieveInput year day test = do
  home <- getHomeDirectory
  if test
    then readFile $
         home ++ testPath ++ show year ++ "/day" ++ show day ++ ".txt"
    else remoteInput year day

remoteInput :: Int -> Int -> IO String
remoteInput year day = do
  let url = adventURL ++ show year ++ "/day/" ++ show day ++ "/input"
  home <- getHomeDirectory
  cookie <- parseCookie $ home ++ cookiePath
  proxy <- readFile (home ++ proxyPath)
--  (code, rsp) <- curlGetString url [CurlProxy (init proxy), CurlCookie cookie]
  (code, rsp) <- curlGetString url [CurlCookie cookie]
  return rsp
