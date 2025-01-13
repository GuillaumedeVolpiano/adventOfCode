module Day11
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.Void
import           FlatParse.Basic           (Result (OK), isLatinLetter, many,
                                            runParser, satisfy)
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

type Parser = F.Parser String

simpleParser :: Parser
simpleParser = reverse <$> many (satisfy isLatinLetter)

next :: Char -> Char
next 'h' = 'j'
next 'k' = 'm'
next 'n' = 'p'
next i   = succ i

nextValidPassword :: String -> String
nextValidPassword password
  | isValid password' = password'
  | otherwise = nextValidPassword password'
  where
    password' = nextPassword password

nextPassword :: String -> String
nextPassword ('z':cs) = 'a' : nextPassword cs
nextPassword (c:cs)   = next c : cs

isValid :: String -> Bool
isValid password = increasePassword password && hasTwoPairs password
  where
    increasePassword [a, b] = False
    increasePassword (a:xs@(b:c:_)) =
      (b == succ c && a == succ b) || increasePassword xs
    hasTwoPairs [a, b, c] = False
    hasTwoPairs (a:xs@(b:xs'))
      | a == b = hasOtherPair xs'
      | otherwise = hasTwoPairs xs
    hasOtherPair [a]          = False
    hasOtherPair (a:xs@(b:_)) = a == b || hasOtherPair xs

part1 :: Bool -> ByteString -> String
part1 _ = reverse . nextValidPassword . extract . runParser simpleParser

part2 :: Bool -> ByteString -> String
part2 _ =
  reverse
    . nextValidPassword
    . nextValidPassword
    . extract
    . runParser simpleParser
