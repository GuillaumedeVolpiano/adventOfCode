module Day22
  ( part1
  , part2
  ) where

import           Control.Monad              (void)
import           Data.Bits                  (shiftL, shiftR, (.&.))
import           Data.Char                  (isAlpha, isSymbol)
import           Data.Either                (fromRight)
import           Data.IntMap                (IntMap, elems, insert, keys, (!))
import qualified Data.IntMap                as M (empty, filter)
import           Data.List                  (foldl', intersperse)
import           Data.Text                  (Text)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (empty, eof, manyTill, parse,
                                             takeWhile1P, (<|>))
import           Text.Megaparsec.Char       (char, eol, letterChar)
import qualified Text.Megaparsec.Char.Lexer as MCL (decimal, lexeme, space)

import           Debug.Trace

type Disk = IntMap (Used, Available)

type Used = Int

type Available = Int

sc :: Parser ()
sc = void . takeWhile1P Nothing $ (\x -> isAlpha x || x `elem` " @-#/%")

space :: Parser ()
space = MCL.space sc empty empty

lexeme = MCL.lexeme space

decimal = lexeme MCL.decimal

parseInput :: Parser Disk
parseInput = do
  manyTill sc eol
  manyTill sc eol
  parseDisk

parseDisk :: Parser Disk
parseDisk = parsePartition <|> (eof >> return M.empty)

parsePartition :: Parser Disk
parsePartition = do
  sc
  x <- decimal
  y <- decimal
  void decimal
  used <- decimal
  available <- decimal
  void decimal
  eol
  insert (x + shiftL y 6) (used, available) <$> parseDisk

countPairs ::
     (Int, [(Used, Available)])
  -> (Used, Available)
  -> (Int, [(Used, Available)])
countPairs (acc, xs) file = (acc + foldr (countPair file) 0 xs, tail xs)

countPair :: (Used, Available) -> (Used, Available) -> Int -> Int
countPair (u1, a1) (u2, a2)
  | u2 /= 0 && a1 >= u2 && u1 /= 0 && a2 >= u1 = (+ 2)
  | (u2 /= 0 && a1 >= u2) || (u1 /= 0 && a2 >= u1) = (+ 1)
  | otherwise = id

findPairs :: Disk -> Int
findPairs disk = fst . foldl' countPairs (0, tail xs) $ xs
  where
    xs = elems disk

prettyPrint :: Disk -> String
prettyPrint disk =
  "  "
    ++ concatMap prettyLine [0 .. mX]
    ++ "\n"
    ++ unlines
         [ prettyNumber y
           ++ concat [prettyPrintChar (x + shiftL y 6) | x <- [0 .. mX]]
         | y <- [0 .. mY]
         ]
    ++ show (fastCalc mX)
    ++ " steps\n"
  where
    vacant = head . keys . M.filter ((== 0) . fst) $ disk
    available = snd $ disk ! vacant :: Int
    mX = maximum . map (.&. 63) . keys $ disk
    mY = maximum . map (`shiftR` 6) . keys $ disk
    prettyPrintChar v
      | v == vacant = "  _"
      | v == 0 = "  G"
      | v == mX = "  S"
      | fst (disk ! v) > available = "  #"
      | otherwise = "  ."
    prettyLine x
      | x <= 10 = "  " ++ show x
      | otherwise = " " ++ show x
    prettyNumber y
      | y < 10 = " " ++ show y
      | otherwise = show y

-- prettyPrinting shows that the empty node (sitting at (3, 20)) just needs to go through a bar of
-- overfull nodes (y == 7). To do that, we need to move the empty node 3 steps
-- to the left, then 20 steps up, then mX steps right. Once that's done, all we
-- need is to go around the goal node (mX - 1) times, until we bring it to (0, 0). It takes 5 steps to go around and substitute.
fastCalc :: Int -> Int
fastCalc mX = 3 + 20 + mX + 5 * (mX - 1)

part1 :: Bool -> Text -> String
part1 _ =
  show
    . findPairs
    . fromRight (error "parse failed")
    . parse parseInput "Day 22"

part2 :: Bool -> Text -> String
part2 _ =
  prettyPrint . fromRight (error "parse failed") . parse parseInput "Day22"
