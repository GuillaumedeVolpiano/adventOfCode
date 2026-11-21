{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Helpers.General.Streamly
  ( preciseTimeIt
  , customPreciseTimeIt
  , retrieveInput
  , readInput
  , fetchInput
  , wallTimeIt
  , isDigit
  , digit
  ) where

import           Control.Monad                (unless, when)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Time.Clock              (diffUTCTime, getCurrentTime)
import           System.TimeIt                (timeItT)
import           Text.Printf                  (printf)

import           Control.Applicative          ((<|>))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS (drop, intercalate,
                                                     isPrefixOf, isSuffixOf,
                                                     readFile, writeFile)
import qualified Data.ByteString.Char8        as BS (readInt)
import qualified Data.ByteString.Char8        as C (pack)
import           Data.Maybe                   (catMaybes)
import qualified Data.Text                    as T (unpack)
import qualified Data.Text.Encoding           as T (decodeUtf8)
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Data.Word                    (Word8)
import           Data.Word8                   (_cr, _lf, _numbersign, _tab)
import           Effectful                    (Eff, IOE, runEff, (:>))
import           Effectful.Error.Static       (runErrorNoCallStack)
import           Effectful.HTTP               (Http, HttpFailure,
                                               Response (..), Scheme (Https),
                                               defaultLimits, fetch, get,
                                               withHeaders)
import           Effectful.HTTP.TLS           (Certificates (SystemCertificates),
                                               runHttp)
import           Helpers.Input                (Input, fromByteString)
import           Helpers.Repository           (inputFile, rootFile, testFile,
                                               userAgent)
import qualified Streamly.Data.Fold           as F (drain, toList)
import qualified Streamly.Data.Parser         as P (many, satisfy, takeWhile)
import           Streamly.Data.Parser         (ParseError (ParseError), Parser)
import qualified Streamly.Data.Stream         as S (parse)
import           Streamly.Data.Stream         (Stream)
import qualified Streamly.External.ByteString as SBS (write)
import qualified Streamly.FileSystem.FileIO   as S (read)
import qualified Streamly.FileSystem.Path     as S (fromString_)
import qualified System.Directory             as SD (doesFileExist)

terminators :: [Word8]
terminators = [_tab]

eols :: [Word8]
eols = [_lf, _cr]

allEnds :: [Word8]
allEnds = terminators ++ eols

isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

digit :: Word8 -> Int
digit w = fromIntegral (w - 48)
{-# INLINE digit #-}

adventHost :: String
adventHost = "adventofcode.com"

-- | A line of cookies.txt, in the Netscape format: what is needed to know
-- whether the cookie goes to a host, and to send it.
data Cookie = Cookie
  { cookieDomain  :: ByteString
  , cookieExpiry  :: Int
  -- ^ Seconds since the epoch; 0 for a cookie that lasts the session.
  , cookieName    :: ByteString
  , cookieValue   :: ByteString
  }

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

parseCookies :: Parser Word8 IO [Cookie]
parseCookies = catMaybes <$> P.many parseLine F.toList

parseLine :: Parser Word8 IO (Maybe Cookie)
parseLine = parseComment <|> parseEmpty <|> parseCookie

-- domain, subdomains too?, path, secure?, expiry, name, value
parseCookie :: Parser Word8 IO (Maybe Cookie)
parseCookie = do
  domain <- normaliseDomain <$> parseChunk
  P.many (P.satisfy (`elem` terminators)) F.drain
  _subdomains <- parseChunk
  _path <- parseChunk
  _secure <- parseChunk
  expiry <- parseInt
  name <- parseChunk
  value <- parseChunk
  pure . Just $ Cookie domain expiry name value

normaliseDomain :: ByteString -> ByteString
normaliseDomain bs = if "." `BS.isPrefixOf` bs then BS.drop 1 bs else bs

parseChunk :: Parser Word8 IO ByteString
parseChunk = P.takeWhile (`notElem` allEnds) SBS.write >>= \bs ->  P.many (P.satisfy (`elem` allEnds)) F.drain >> pure bs

parseComment :: Parser Word8 IO (Maybe Cookie)
parseComment = P.many (P.satisfy (`elem` terminators)) F.drain >> P.satisfy (== _numbersign ) >> P.many (P.satisfy (`notElem` eols)) F.drain >> P.satisfy (`elem` eols) >> pure Nothing

parseInt :: Parser Word8 IO Int
parseInt = do
  val <- parseChunk
  let mi = BS.readInt val
  case mi of
    Nothing -> error $ "parsing of" ++ (T.unpack . T.decodeUtf8 $ val) ++ "failed"
    Just (raw, _) -> pure raw

parseEmpty :: Parser Word8 IO (Maybe Cookie)
parseEmpty = P.many (P.satisfy (`elem` terminators)) F.drain >> P.satisfy (`elem` eols) >> pure Nothing

-- | The value of the Cookie header for a host: the cookies of cookies.txt
-- that belong to it or to a domain above it, and have not expired.
cookieHeader :: String -> IO ByteString
cookieHeader host = do
  cookieName' <- rootFile "cookies.txt"
  parsed <- S.parse parseCookies . S.read . S.fromString_ $ cookieName'
  now <- round <$> getPOSIXTime
  let cookies = case parsed of
        Left (ParseError s) -> error $ "Could not parse cookie jar " ++ s
        Right c             -> c
      host' = C.pack host
      matches c =
        (cookieDomain c == host' || ("." <> cookieDomain c) `BS.isSuffixOf` host')
          && (cookieExpiry c == 0 || cookieExpiry c > now)
      sent = [cookieName c <> "=" <> cookieValue c | c <- cookies, matches c]
  when (null sent) . fail $
    "no valid cookie for " ++ host ++ " in " ++ cookieName'
  pure $ BS.intercalate "; " sent

-- | The input of a day as a stream read from its file as it is consumed.
retrieveInput :: Int -> Int -> Bool -> IO (Stream IO Word8)
retrieveInput year day test = S.read . S.fromString_ <$> localInput year day test

-- | The input of a day, whole in memory.
readInput :: Int -> Int -> Bool -> IO Input
readInput year day test = fromByteString <$> (localInput year day test >>= BS.readFile)

-- | The file that holds the input of a day, or its example, downloaded
-- first when it is the input and it is not there yet.
localInput :: Int -> Int -> Bool -> IO FilePath
localInput year day test = do
  inputName <- inputFile year day
  testName <- testFile year day
  let file
        | test = testName
        | otherwise =
          inputName
  exist <- SD.doesFileExist file
  unless (test || exist) $ remoteInput year day file
  pure file

-- | Download the input of a day to a file, over the Http effect of
-- effectful-streamly-http, interpreted over TLS. The file is only written
-- once the whole input has arrived.
remoteInput :: Int -> Int -> FilePath -> IO ()
remoteInput year day file = do
  outcome <-
    runEff
      . runErrorNoCallStack @HttpFailure
      . runHttp SystemCertificates defaultLimits
      $ fetchInput year day
  case outcome of
    Left failure -> fail $ "failed to fetch input: " ++ show failure
    Right input  -> BS.writeFile file input

-- | The input of a day, asked from Advent of Code with the session cookie
-- of cookies.txt.
fetchInput :: (IOE :> es, Http :> es) => Int -> Int -> Eff es ByteString
fetchInput year day = do
  cookies <- liftIO $ cookieHeader adventHost
  response <-
    fetch Https
      . withHeaders [("User-Agent", C.pack userAgent), ("Cookie", cookies)]
      $ get adventHost 443 target
  if response.status == 200
    then pure response.body
    else liftIO . fail $
           "failed to fetch input, status: " ++ show response.status
  where
    target = C.pack $ "/" ++ show year ++ "/day/" ++ show day ++ "/input"
