module Helpers.General.ByteString
  ( preciseTimeIt
  , customPreciseTimeIt
  , retrieveInput
  , wallTimeIt
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           System.TimeIt          (timeItT)
import           Text.Printf            (printf)

import           Data.Bifunctor         (second)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS (putStr, readFile, writeFile)
import           Data.ByteString.UTF8   (fromString)
import           Data.List.Split        (splitOn)
import           Helpers.Repository      (inputFile, rootFile, testFile,
                                         userAgent)
import           Network.Curl           (CurlOption (CurlCookie, CurlProxy,
                                                     CurlUserAgent),
                                         curlGetString)
import           System.Directory       (doesFileExist)

adventURL = "https://adventofcode.com/"

preciseTimeIt :: (MonadIO m, Show a) => Int -> m a -> m a
preciseTimeIt = customPreciseTimeIt "CPU time: "

customPreciseTimeIt :: (MonadIO m, Show a) => String -> Int -> m a -> m a
customPreciseTimeIt name prec ioa = do
  (t, a) <- timeItT ioa
  liftIO . printf (name ++ " CPU Time: %6." ++ show prec ++ "fs\n") $ t
  return a

wallTimeIt :: (MonadIO m, Show a) => String -> Int -> m a -> m a
wallTimeIt name prec ioa = do
  (t, a) <- wallTimeItT ioa
  liftIO . printf (name ++ " Wall Time: %6." ++ show prec ++ "fs\n") $ t
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

retrieveInput :: Int -> Int -> Bool -> Bool -> Bool -> IO ByteString
retrieveInput year day test local withProxy = do
  inputName <- inputFile year day
  testName <- testFile year day
  let file
        | test = testName
        | otherwise =
          inputName
  exists <- doesFileExist file
  when (not test && not exists) $ remoteInput year day withProxy file
  if test
    then BS.readFile file
    else BS.readFile
           $ inputName

remoteInput :: Int -> Int -> Bool -> FilePath -> IO ()
remoteInput year day withProxy file = do
  let url = adventURL ++ show year ++ "/day/" ++ show day ++ "/input"
  cookieName <- rootFile "cookies.json"
  proxyName <- rootFile "proxy"
  cookie <- parseCookie $ cookieName
  proxy <- readFile (proxyName)
  let curlArgs =
        if withProxy
          then [CurlProxy (init proxy), CurlCookie cookie, CurlUserAgent userAgent]
          else [CurlCookie cookie, CurlUserAgent userAgent]
  (code, rsp) <- second fromString <$> curlGetString url curlArgs
  BS.writeFile file rsp
