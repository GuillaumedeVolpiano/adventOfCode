module Main where

import           Control.Monad.State   (evalState)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B (putStr, readFile)
import           Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as C (putStrLn)
import           Data.Either           (fromRight)
import qualified Day6
import           System.Directory      (getHomeDirectory)
import           Test.Tasty.Bench      (Benchmark, bcompare, bench, bgroup,
                                        defaultMain, nf, nfIO)
import           Text.Megaparsec       (runParserT)

inputPath = "/adventOfCode/input/2015/day6.txt"

test = replicate 30000 'a' :: String

op x = flip evalState x . runParserT Day6.parseInput "Benching"

tests :: ByteString -> [Benchmark]
tests input =
  [ bench "simple map" $ nf (op Day6.dlempty) input
  , bench "intmap" $ nf (op Day6.bdlempty) input
  , bench "intmultiset" $ nf (op Day6.obdlempty) input
  ]

testIO =
  [ bench "String" $ nfIO (putStrLn test)
  , bench "ByteString"
      $ nfIO ((B.putStr . pack $ test) >> (B.putStr . pack $ "\n"))
  , bench "Char8" $ nfIO (C.putStrLn . pack $ test)
  ]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- B.readFile $ home ++ inputPath
  defaultMain testIO --s file
