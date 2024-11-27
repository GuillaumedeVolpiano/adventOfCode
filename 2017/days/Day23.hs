module Day23
  ( part1
  , part2
  ) where

import           Data.Char                        (isAlpha)
import           Data.Map                         as M (Map, adjust, empty,
                                                        findWithDefault, insert,
                                                        singleton)
import           Data.Maybe                       (fromJust, isNothing)
import           Data.Sequence                    as S (Seq ((:<|), (:|>)),
                                                        empty, length, null)
import           Data.Vector                      (Vector, fromList, (!?))
import           Helpers.Parsers                  (Parser, parseByLine)
import           Math.NumberTheory.Primes.Testing (isPrime)
import           Text.Megaparsec                  (many, manyTill, optional,
                                                   sepBy, (<|>))
import           Text.Megaparsec.Char             (alphaNumChar, char, eol,
                                                   spaceChar, string)
import           Text.Megaparsec.Debug

data Op
  = Set Char String
  | Sub Char String
  | Mul Char String
  | Jnz String String
  deriving (Show)

type Program = Vector Op

data Computer = Computer
  { counter :: Int
  , pointer :: Int
  , memory  :: Map Char Int
  , program :: Program
  } deriving (Show)

alnum :: Parser [String]
alnum = manyTill alnumChars eol

alnumChars :: Parser String
alnumChars = do
  val <- many (char '-' <|> alphaNumChar)
  optional . char $ ' '
  return val

parseOp :: Parser Op
parseOp = set <|> sub <|> mul <|> jnz

set :: Parser Op
set = do
  string "set "
  [v1:_, v2] <- alnum
  optional eol
  return . Set v1 $ v2

sub :: Parser Op
sub = do
  string "sub "
  [v1:_, v2] <- alnum
  optional eol
  return . Sub v1 $ v2

mul :: Parser Op
mul = do
  string "mul "
  [v1:_, v2] <- alnum
  optional eol
  return . Mul v1 $ v2

jnz :: Parser Op
jnz = do
  string "jnz "
  [v1, v2] <- alnum
  return . Jnz v1 $ v2

execute1 :: Computer -> Int
execute1 computer
  | isNothing safeInst = counter computer
  | otherwise = execute1 . execute inst $ computer
  where
    safeInst = program computer !? pointer computer
    inst = fromJust safeInst

execute :: Op -> Computer -> Computer
execute inst computer = computer' {pointer = pointer'}
  where
    pointer' = posInst inst
    computer' = operate inst computer
    posInst (Jnz v1 v2)
      | val v1 computer /= 0 = pointer computer + val v2 computer
    posInst _ = pointer computer + 1

val :: String -> Computer -> Int
val v computer
  | all isAlpha v = findWithDefault 0 (head v) . memory $ computer
  | otherwise = read v

operate :: Op -> Computer -> Computer
operate (Jnz _ _) computer = computer
operate (Set v1 v2) computer =
  computer {memory = insert v1 (val v2 computer) . memory $ computer}
operate (Sub v1 v2) computer =
  computer {memory = adjust (flip (-) (val v2 computer)) v1 . memory $ computer}
operate (Mul v1 v2) computer =
  computer
    { counter = counter computer + 1
    , memory = adjust (* val v2 computer) v1 . memory $ computer
    }

part1 :: Bool -> String -> String
part1 _ =
  show . execute1 . Computer 0 0 M.empty . fromList . parseByLine parseOp

part2 :: Bool -> String -> String
part2 _ _ =
  show . Prelude.length . filter (not . isPrime) $ [b,b + 17 .. b + 17000]
  where
    b = 100000 + 9900
