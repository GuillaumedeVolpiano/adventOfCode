{-# LANGUAGE OverloadedStrings #-}
module Helpers.General.Streamly
  ( preciseTimeIt
  , customPreciseTimeIt
  , retrieveInput
  , wallTimeIt
  ) where

import           Control.Monad                (unless, void)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Time.Clock              (UTCTime, diffUTCTime,
                                               getCurrentTime)
import           System.TimeIt                (timeItT)
import           Text.Printf                  (printf)

import           Control.Applicative          ((<|>))
import           Data.ByteString              (ByteString)
import           Data.ByteString              as BS (drop, isPrefixOf, null)
import qualified Data.ByteString.Char8        as BS (readInt)
import           Data.Function                ((&))
import           Data.Maybe                   (catMaybes)
import qualified Data.Text                    as T (unpack)
import qualified Data.Text.Encoding           as T (decodeUtf8)
import           Data.Time.Clock.POSIX        (posixSecondsToUTCTime)
import           Data.Word                    (Word8)
import           Data.Word8                   (_colon, _cr, _lf, _numbersign,
                                               _slash, _tab)
import           Network.HTTP.Client          (BodyReader, Cookie (Cookie),
                                               CookieJar, Proxy (Proxy),
                                               Response, brRead,
                                               createCookieJar,
                                               insertCookiesIntoRequest,
                                               parseUrlThrow, requestHeaders,
                                               responseBody,
                                               responseStatus, withResponse)
import qualified Network.HTTP.Client          as H (proxy)
import           Network.HTTP.Client.TLS      (newTlsManager)
import           Network.HTTP.Types           (statusCode)
import qualified Streamly.Data.Fold           as F (drain, toList)
import qualified Streamly.Data.Parser         as P (many, satisfy, takeWhile)
import           Streamly.Data.Parser         (ParseError (ParseError), Parser)
import qualified Streamly.Data.Stream         as S (concatMap, fold, parse,
                                                    unfold)
import           Streamly.Data.Stream         (Stream)
import qualified Streamly.Data.Unfold         as U (unfoldrM)
import           Streamly.Data.Unfold         (Unfold)
import qualified Streamly.External.ByteString as SBS (reader, write)
import qualified Streamly.FileSystem.FileIO   as S (read, write)
import qualified Streamly.FileSystem.Path     as S (fromString_)
import           Streamly.FileSystem.Path     (Path)
import qualified System.Directory             as SD (doesFileExist)
import           System.Directory             (getHomeDirectory)

terminators :: [Word8]
terminators = [_tab]

eols :: [Word8]
eols = [_lf, _cr]

allEnds :: [Word8]
allEnds = terminators ++ eols

cookiePath :: FilePath
cookiePath = "/github/adventOfCode/cookies.txt"

adventURL :: FilePath
adventURL = "https://adventofcode.com/"

testPath :: FilePath
testPath = "/github/adventOfCode/test/"

inputPath :: FilePath
inputPath = "/github/adventOfCode/input/"

proxyPath :: FilePath
proxyPath = "/github/adventOfCode/proxy"

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

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

parseCookies :: Parser Word8 IO CookieJar
parseCookies = createCookieJar . catMaybes <$> P.many parseLine F.toList

parseLine :: Parser Word8 IO (Maybe Cookie)
parseLine = parseComment <|> parseEmpty <|> parseCookie

parseCookie :: Parser Word8 IO (Maybe Cookie)
parseCookie = do
  domain <- normaliseDomain <$> parseChunk
  P.many (P.satisfy (`elem` terminators)) F.drain
  hostOnly <- not <$> parseBool
  path <- parseChunk
  secure <- parseBool
  expiry <- parseDate
  name <- parseChunk
  value <- parseChunk
  pure . Just $ Cookie name value expiry domain path epoch epoch True hostOnly secure True

normaliseDomain :: ByteString -> ByteString
normaliseDomain bs = if "." `BS.isPrefixOf` bs then BS.drop 1 bs else bs

parseChunk :: Parser Word8 IO ByteString
parseChunk = P.takeWhile (`notElem` allEnds) SBS.write >>= \bs ->  P.many (P.satisfy (`elem` allEnds)) F.drain >> pure bs

parseBool :: Parser Word8 IO Bool
parseBool = (== "TRUE") <$> parseChunk

parseComment :: Parser Word8 IO (Maybe Cookie)
parseComment = P.many (P.satisfy (`elem` terminators)) F.drain >> P.satisfy (== _numbersign ) >> P.many (P.satisfy (`notElem` eols)) F.drain >> P.satisfy (`elem` eols) >> pure Nothing

parseDate :: Parser Word8 IO UTCTime
parseDate = posixSecondsToUTCTime . fromIntegral <$> parseInt

parseInt :: Parser Word8 IO Int
parseInt = do
  val <- parseChunk
  let mi = BS.readInt val
  case mi of
    Nothing -> error $ "parsing of" ++ (T.unpack . T.decodeUtf8 $ val) ++ "failed"
    Just (raw, _) -> pure raw

parseEmpty :: Parser Word8 IO (Maybe Cookie)
parseEmpty = P.many (P.satisfy (`elem` terminators)) F.drain >> P.satisfy (`elem` eols) >> pure Nothing

-- to be improved
parseProxy :: Parser Word8 IO Proxy
parseProxy = do
  P.many (P.satisfy (/= _colon)) F.drain
  void $ P.satisfy (== _colon) >> P.satisfy (== _slash) >> P.satisfy (== _slash)
  host <- P.takeWhile (/= _colon) SBS.write
  void $ P.satisfy (== _colon)
  Proxy host <$> parseInt

streamResponse :: Response BodyReader -> Stream IO Word8
streamResponse response = S.unfold unfoldBodyReader (responseBody response) & S.concatMap (S.unfold SBS.reader)

unfoldBodyReader :: Unfold IO BodyReader ByteString
unfoldBodyReader = U.unfoldrM $ \body -> do
  chunk <- brRead body
  if BS.null chunk then pure Nothing
                   else pure . Just $ (chunk, body)


retrieveInput :: Int -> Int -> Bool -> Bool -> IO (Stream IO Word8)
retrieveInput year day test withProxy = do
  home <- getHomeDirectory
  let file
        | test = home ++ testPath ++ show year ++ "/day" ++ show day ++ ".txt"
        | otherwise =
          home ++ inputPath ++ show year ++ "/day" ++ show day ++ ".txt"
  exist <- SD.doesFileExist file
  unless (test || exist) $ remoteInput year day withProxy $ S.fromString_ file
  pure . S.read . S.fromString_ $ file

remoteInput :: Int -> Int -> Bool -> Path -> IO ()
remoteInput year day withProxy file = do
  let url = adventURL ++ show year ++ "/day/" ++ show day ++ "/input"
  req <- parseUrlThrow url
  home <- getHomeDirectory
  eCookieJar <- S.parse parseCookies . S.read . S.fromString_ $ home ++ cookiePath
  eProxy <- S.parse parseProxy . S.read . S.fromString_  $ home ++ proxyPath
  now <- getCurrentTime
  let cookieJar = case eCookieJar of
                    Left (ParseError s) -> error $ "Could not parse cookie jar " ++ s
                    Right c -> c
      proxy = case eProxy of
                    Left (ParseError s) -> error $ "Could not parse proxy" ++ s
                    Right p             -> if withProxy then Just p else Nothing
      fullReq = fst $ insertCookiesIntoRequest (req {H.proxy = proxy, requestHeaders = ("User-Agent", "curl/8.5.0"):requestHeaders req}) cookieJar now
  manager <- newTlsManager
  withResponse fullReq manager $ \rsp -> do
    let code = statusCode $ responseStatus rsp
    if code == 200 then streamResponse rsp & S.fold (S.write file)
                   else error $ "failed to fetch input, status: " ++ show code
