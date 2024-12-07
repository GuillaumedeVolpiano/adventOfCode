module Helpers.General.Text
  ( preciseTimeIt
  , customPreciseTimeIt
  , retrieveInput
  , wallTimeIt
  ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           System.TimeIt          (timeItT)
import           Text.Printf            (printf)

import           Data.List.Split        (splitOn)
import           Data.Text              as T (Text, pack, concat)
import qualified Data.Text.IO.Utf8      as TIO (readFile, putStr)
import           Network.Curl           (CurlOption (CurlCookie, CurlProxy),
                                         curlGetString)
import           System.Directory       (getHomeDirectory)
import TextShow (showt, TextShow)

cookiePath = "/adventOfCode/cookies.json"

adventURL = "https://adventofcode.com/"

testPath = "/adventOfCode/test/"

inputPath = "/adventOfCode/input/"

proxyPath = "/adventOfCode/proxy"

preciseTimeIt :: (MonadIO m, TextShow a) => Int -> m a -> m a
preciseTimeIt = customPreciseTimeIt . pack $ "CPU time: "

customPreciseTimeIt :: (MonadIO m, TextShow a) => Text -> Int -> m a -> m a
customPreciseTimeIt name prec ioa = do
  (t, a) <- timeItT ioa
  liftIO . TIO.putStr $ T.concat [name, pack . printf " CPU Time: %6." $ t, showt prec, pack "fs\n"]
  return a

wallTimeIt :: (MonadIO m, TextShow a) => Text -> Int -> m a -> m a
wallTimeIt name prec ioa = do
  (t, a) <- wallTimeItT ioa
  liftIO . TIO.putStr $ T.concat [name, pack . printf " Wall Time: %6." $ t, showt prec, pack "fs\n"]
  return a

wallTimeItT :: (MonadIO m) => m a -> m (Double, a)
wallTimeItT ioa = do
  t1 <- liftIO getCurrentTime
  a <- ioa
  t2 <- liftIO getCurrentTime
  let t :: Double
      t = realToFrac (diffUTCTime t2 t1)
  return (t, a)

parseCookie :: String -> IO String
parseCookie path = do
  rawCookie <- readFile path
  let cookieLength = length rawCookie
  let (name:value:_) =
        map (\(_:y:_) -> init y)
          . filter (\(x:_) -> x == "name" || x == "value")
          . map (splitOn ":")
          . splitOn "\n"
          . filter (\x -> x /= ' ' && x /= '\"')
          . drop 4
          . take (cookieLength - 2)
          $ rawCookie
  return $ name ++ "=" ++ value

retrieveInput :: Int -> Int -> Bool -> Bool -> Bool -> IO Text
retrieveInput year day test local withProxy = do
  home <- getHomeDirectory
  if test
    then TIO.readFile
           $ home ++ testPath ++ show year ++ "/day" ++ show day ++ ".txt"
    else if local
           then TIO.readFile
                  $ home
                      ++ inputPath
                      ++ show year
                      ++ "/day"
                      ++ show day
                      ++ ".txt"
           else remoteInput year day withProxy

remoteInput :: Int -> Int -> Bool -> IO Text
remoteInput year day withProxy = do
  let url = adventURL ++ show year ++ "/day/" ++ show day ++ "/input"
  home <- getHomeDirectory
  cookie <- parseCookie $ home ++ cookiePath
  proxy <- readFile (home ++ proxyPath)
  let curlArgs =
        if withProxy
          then [CurlProxy (init proxy), CurlCookie cookie]
          else [CurlCookie cookie]
  (code, rsp) <- curlGetString url curlArgs
  return . pack $ rsp
