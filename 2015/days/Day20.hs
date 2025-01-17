module Day20
  ( part1
  , part2
  ) where

import           Data.Bifunctor            (first)
import           Data.ByteString           (ByteString)
import           Data.HashMap.Strict       (HashMap, fromList)
import qualified Data.HashMap.Strict       as H (lookup)
import           Data.List                 (unfoldr)
import           Data.Maybe                (fromJust, isNothing)
import           FlatParse.Basic           (anyAsciiDecimalInt, runParser)
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)
import           Math.NumberTheory.Primes  (Prime, factorise, unPrime)

import           Debug.Trace

fromPrimes :: Int -> Int -> Int
fromPrimes maxVal k
  | sumFactPrimes >= maxVal = k
  | otherwise = fromPrimes maxVal (k + 1)
  where
    factPrimes = factorise k
    sumFactPrimes = product . map geoSum $ factPrimes
    geoSum (fact, occ) =
      div (unPrime fact ^ (fromIntegral occ + 1) - 1) (unPrime fact - 1)

fromLimitedPrimes :: Int -> Int -> HashMap Int Word -> Int
fromLimitedPrimes maxVal k divvedFacts
  | sumFactPrimes > maxVal = k
  | otherwise = fromLimitedPrimes maxVal (k + 1) divvedFacts
  where
    factPrimes = factorise k
    sumFactPrimes = product . map geoSum $ factPrimes
    geoSum (fact, occ) = div (shortened (unPrime fact) occ) (unPrime fact - 1)
    shortened fact occ
      | limFact > Just occ || isNothing limFact = fact ^ (occ + 1) - 1
      | otherwise = fact ^ (occ + 1) - (fact ^ fromJust limFact)
      where
        limFact = H.lookup fact divvedFacts

initLimitedPrimes :: Int -> Int
initLimitedPrimes maxVal =
  fromLimitedPrimes maxVal (ceiling . sqrt . fromIntegral $ 2 * maxVal)
    $ fromList . map (first unPrime)
    $ factorise (div maxVal 50)

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . flip fromPrimes 10000
    . flip div 10
    . extract
    . runParser anyAsciiDecimalInt

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . initLimitedPrimes
    . flip div 10
    . extract
    . runParser anyAsciiDecimalInt
