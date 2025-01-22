{-# LANGUAGE TemplateHaskell #-}

module Day12
  ( part1
  , part2
  ) where

import           Control.Monad                          (void)
import           Control.Parallel.Strategies            (parMap, rpar)
import           Data.Bifunctor                         (bimap, second)
import           Data.ByteString                        (ByteString)
import           Data.List                              (group, intercalate,
                                                         sort)
import           Data.Maybe                             (catMaybes)
import           Data.Vector.Unboxed                    (Vector, (!))
import qualified Data.Vector.Unboxed                    as V (fromList, length)
import           FlatParse.Basic                        (anyAsciiDecimalInt,
                                                         char, eof, many,
                                                         optional_, runParser,
                                                         satisfy, some)
import           Helpers.Parsers.FlatParse              (Parser, extract)
import           Math.NumberTheory.Recurrences.Bilinear (binomialLine)
import qualified Streamly.Data.Stream                   as S (fromList, mapM)
import           Streamly.Data.Stream                   (Stream)

data State = State
  { curGroup     :: Int
  , amount       :: Int
  , permutations :: Int
  } deriving (Show)

instance Eq State where
  (State g1 a1 _) == (State g2 a2 _) = g1 == g2 && a1 == a2

instance Ord State where
  compare (State g1 a1 p1) (State g2 a2 p2) =
    compare g1 g2 `mappend` compare a1 a2 `mappend` compare p1 p2

type Record = (Condition, Contiguous)

type Condition = String

type Contiguous = Vector Int

parseLine :: Parser (String, [Int])
parseLine =
  some (satisfy (`elem` "#?.")) >>= \s ->
    $(char ' ')
      >> many (anyAsciiDecimalInt >>= \i -> optional_ $(char ',') >> pure i)
      >>= \is -> $(char '\n') >> pure (s, is)

parseInput :: Parser [(String, [Int])]
parseInput = many parseLine <* eof

parseNoDots :: Parser String
parseNoDots =
  consumeDots >> some (satisfy (`elem` "#?")) >>= \s -> consumeDots >> pure s

parseDots :: Parser String
parseDots = some (satisfy (`elem` "#?."))

consumeDots :: Parser ()
consumeDots = void . many $ $(char '.')

rakeStream :: [State] -> Record -> [Int]
rakeStream states record = [rake states record]

rake :: [State] -> Record -> Int
rake states ([], cont) =
  sum
    . map permutations
    . filter
        (\(State g a _) ->
           g == V.length cont || g == (V.length cont - 1) && a == cont ! g)
    $ states
rake states (s:ss, cont) = rake combined (ss, cont)
  where
    raked = concatMap (catMaybes . nextChar s cont) states
    combined =
      map (foldl1 (\(State g a p1) (State _ _ p2) -> State g a (p1 + p2)))
        . group
        . sort
        $ raked

nextChar :: Char -> Contiguous -> State -> [Maybe State]
nextChar '?' cont state = nextChar '.' cont state ++ nextChar '#' cont state
nextChar '.' cont s@(State g a p)
  | g == V.length cont || a == 0 = [Just s]
  | a == cont ! g = [Just $ State (g + 1) 0 p]
  | otherwise = [Nothing]
nextChar '#' cont s@(State g a p)
  | g >= V.length cont || a == cont ! g = [Nothing]
  | otherwise = [Just $ State g (a + 1) p]

part1 :: Bool -> ByteString -> String
part1 _ input = show . sum . S.mapM (rakeStream [State 0 0 1]) $ pairs
  where
    pairs =
      S.fromList . map (second V.fromList) . extract . runParser parseInput
        $ input

part2 :: Bool -> ByteString -> String
part2 _ input = show . sum . S.mapM (rakeStream [State 0 0 1]) $ pairs
  where
    pairs =
      S.fromList
        . map (bimap unfoldSpring unfoldRecord)
        . extract
        . runParser parseInput
        $ input
    unfoldSpring = intercalate "?" . replicate 5
    unfoldRecord = V.fromList . concat . replicate 5
