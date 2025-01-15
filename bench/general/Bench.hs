module Main where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (putStr, readFile)
import qualified Data.ByteString.Char8      as BC (lines, unpack)
import           Data.ByteString.UTF8       (toString)
import           Data.Massiv.Array          (Array, Comp (Seq), P, fromLists')
import           Data.Massiv.Core.Index     (Ix2)
import           Data.Text.Encoding         (decodeUtf8)
import           Day12
import qualified Helpers.Parsers            as P (arrayFromString)
import qualified Helpers.Parsers.ByteString as B (arrayFromByteString)
import qualified Helpers.Parsers.Text       as T (arrayFromText)
import           System.Directory           (getHomeDirectory)
import           Test.Tasty.Bench           (Benchmark, bcompare, bench, bgroup,
                                             defaultMain, nf, nfIO, whnf)

inputPath = "/adventOfCode/input/2015/day18.txt"

tests :: ByteString -> [Benchmark]
tests input =
  [ bench "String" $ whnf (P.arrayFromString . toString) input
  , bench "Text" $ whnf (T.arrayFromText . decodeUtf8) input
  , bench "ByteString" $ whnf B.arrayFromByteString input
  , bench "Massiv"
      $ nf
          ((fromLists' Seq :: [String] -> Array P Ix2 Char)
             . map BC.unpack
             . BC.lines)
          input
  ]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- B.readFile $ home ++ inputPath
  defaultMain . tests $ file
