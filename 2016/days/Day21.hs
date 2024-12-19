module Day21
  ( part1
  , part2
  ) where

import           Data.Either                (fromRight)
import           Data.IntMap                as I (IntMap, assocs, fromList,
                                                  insert, (!))
import           Data.List                  (foldl')
import           Data.Map                   as M (Map, assocs, fromList, insert,
                                                  (!))
import           Data.Text                  (Text, pack)
import           Data.Tuple                 (swap)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (eof, manyTill, optional, parse,
                                             (<|>))
import           Text.Megaparsec.Char       (char, eol, lowerChar, string)
import           Text.Megaparsec.Char.Lexer (decimal)

data Operation
  = SwapPos Int Int
  | SwapLetter Char Char
  | RotateLeft Int
  | RotateRight Int
  | RotatePos Char
  | Reverse Int Int
  | Move Int Int
  deriving (Eq, Ord, Show)

data Hash =
  Hash (IntMap Char) (Map Char Int)

instance Show Hash where
  show (Hash indexMap _) = map snd . I.assocs $ indexMap

password test =
  Hash
    (I.fromList . assocList $ test)
    (M.fromList . map swap . assocList $ test)

assocList test
  | test = zip [0 ..] "abcde"
  | otherwise = zip [0 ..] "abcdefgh"

scrambledList = zip [0 ..] "fbgdceah"

scrambled =
  Hash (I.fromList scrambledList) (M.fromList . map swap $ scrambledList)

hashInsert :: Int -> Char -> Hash -> Hash
hashInsert x a (Hash indexMap charMap) =
  Hash (I.insert x a indexMap) (M.insert a x charMap)

parseInput :: Parser [Operation]
parseInput = manyTill parseOp eof

parseOp :: Parser Operation
parseOp =
  parseSwapPos
    <|> parseSwapLetter
    <|> parseRotateLeft
    <|> parseRotateRight
    <|> parseRotatePos
    <|> parseReverse
    <|> parseMove

parseSwapPos :: Parser Operation
parseSwapPos = do
  string . pack $ "swap position "
  x <- decimal
  string . pack $ " with position "
  y <- decimal
  eol
  return . SwapPos x $ y

parseSwapLetter :: Parser Operation
parseSwapLetter = do
  string . pack $ "swap letter "
  x <- lowerChar
  string . pack $ " with letter "
  y <- lowerChar
  eol
  return . SwapLetter x $ y

parseRotateLeft :: Parser Operation
parseRotateLeft = do
  string . pack $ "rotate left "
  x <- decimal
  string . pack $ " step"
  optional . char $ 's'
  eol
  return . RotateLeft $ x

parseRotateRight :: Parser Operation
parseRotateRight = do
  string . pack $ "rotate right "
  x <- decimal
  string . pack $ " step"
  optional . char $ 's'
  eol
  return . RotateRight $ x

parseRotatePos :: Parser Operation
parseRotatePos = do
  string . pack $ "rotate based on position of letter "
  x <- lowerChar
  eol
  return . RotatePos $ x

parseReverse :: Parser Operation
parseReverse = do
  string . pack $ "reverse positions "
  x <- decimal
  string . pack $ " through "
  y <- decimal
  eol
  return . Reverse x $ y

parseMove :: Parser Operation
parseMove = do
  string . pack $ "move position "
  x <- decimal
  string . pack $ " to position "
  y <- decimal
  eol
  return . Move x $ y

makeOperation :: [Operation] -> (Hash -> Hash)
makeOperation = foldl' (\ops op -> operation op . ops) id

makeInverseOperations :: [Operation] -> (Hash -> Hash)
makeInverseOperations = foldr (\op ops -> invert op . ops) id

hashPassword :: Hash -> [Operation] -> Hash
hashPassword hash operations = makeOperation operations hash

unhashPassword :: Hash -> [Operation] -> Hash
unhashPassword hash operations = makeInverseOperations operations hash

operation :: Operation -> Hash -> Hash
operation (SwapPos x y) hash@(Hash indexMap _) =
  hashInsert y a . hashInsert x b $ hash
  where
    a = indexMap I.! x
    b = indexMap I.! y
operation (SwapLetter a b) hash@(Hash _ charMap) =
  hashInsert y a . hashInsert x b $ hash
  where
    x = charMap M.! a
    y = charMap M.! b
operation (RotateLeft x) (Hash indexMap _) =
  Hash (I.fromList assocsList') (M.fromList . map swap $ assocsList')
  where
    assocsList = I.assocs indexMap
    assocsList' =
      map (\(i, c) -> ((i - x) `mod` length assocsList, c)) assocsList
operation (RotateRight x) (Hash indexMap _) =
  Hash (I.fromList assocsList') (M.fromList . map swap $ assocsList')
  where
    assocsList = I.assocs indexMap
    assocsList' =
      map (\(i, c) -> ((i + x) `mod` length assocsList, c)) assocsList
operation (RotatePos c) hash@(Hash _ charMap) = operation (RotateRight i') hash
  where
    i = charMap M.! c
    i'
      | i >= 4 = i + 2
      | otherwise = i + 1
operation (Reverse x y) hash@(Hash indexMap _) =
  foldr (uncurry hashInsert) hash reversed
  where
    reversed = zip [y,y - 1 .. x] . map (indexMap I.!) $ [x,x + 1 .. y]
operation (Move x y) hash@(Hash indexMap _) =
  foldr (uncurry hashInsert) hash moved
  where
    moved = zip newIndices . map (indexMap I.!) $ movedIndices
    movedIndices
      | x < y = [x + 1 .. y] ++ [x]
      | otherwise = x : [y .. (x - 1)]
    newIndices
      | x < y = [x .. y]
      | otherwise = [y .. x]

invert :: Operation -> Hash -> Hash
invert op@(SwapPos _ _) hash = operation op hash
invert op@(SwapLetter _ _) hash = operation op hash
invert (RotateLeft x) hash = operation (RotateRight x) hash
invert (RotateRight x) hash = operation (RotateLeft x) hash
invert op@(Reverse _ _) hash = operation op hash
invert (Move x y) hash = operation (Move y x) hash
invert (RotatePos c) hash@(Hash _ charMap) = operation (RotateLeft i') hash
  where
    -- if i was greater than 4, then the new index will be 2*i + 2 `mod` 8, so
    -- if i = 4 + k, the new index is going to be 2*k + 2, with 0 <= k <= 3, and
    -- 0 <= new i <= 6, even new i. So we have k = (div (new i) 2 - 1) `mod` 8
    -- and i = 4 + k. We want to rotate (i + 2) to the left, that is 5 + div i'
    -- 2. We need to make a special case for i' == 0, which is actually i' == 8.
    -- As 5 + 4 = 9, that's 1.
    -- if i was less than 4, then the new index is 2*i + 1. We have i = div (new i -
    -- 1) 2, and we want to rotate back i + 1, which is going to be div (new i +
    -- 1) 2
    i = charMap M.! c
    i'
      | i == 0 = 1
      | even i = 5 + div i 2
      | odd i = div (i + 1) 2

part1 :: Bool -> Text -> String
part1 test =
  show
    . hashPassword (password test)
    . fromRight (error "parse failed")
    . parse parseInput "day21"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . unhashPassword scrambled
    . fromRight (error "parse failed")
    . parse parseInput "day21"
