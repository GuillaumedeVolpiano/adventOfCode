module Day18
  ( part1
  , part2
  ) where

import           Data.Char            (isAlpha)
import           Data.Map             as M (Map, adjust, empty, findWithDefault,
                                            insert, singleton)
import           Data.Maybe           (fromJust, isNothing)
import           Data.Sequence        as S (Seq ((:<|), (:|>)), empty, length,
                                            null)
import           Data.Vector          (Vector, fromList, (!?))
import           Helpers.Parsers      (Parser, parseByLine)
import           Text.Megaparsec      (many, manyTill, optional, sepBy, (<|>))
import           Text.Megaparsec.Char (alphaNumChar, char, eol, spaceChar,
                                       string)

data Op
  = Snd String
  | Set Char String
  | Add Char String
  | Mul Char String
  | Mod Char String
  | Rcv Char
  | Jgz String String
  deriving (Show)

type Program = Vector Op

data Computer = Computer
  { counter  :: Int
  , received :: Seq Int
  , send     :: Seq Int
  , pointer  :: Int
  , memory   :: Map Char Int
  , program  :: Program
  } deriving (Show)

alnum :: Parser [String]
alnum = manyTill alnumChars eol

alnumChars :: Parser String
alnumChars = do
  val <- many (char '-' <|> alphaNumChar)
  optional . char $ ' '
  return val

parseOp :: Parser Op
parseOp = sound <|> set <|> add <|> mul <|> rmod <|> rcv <|> jgz

rcv :: Parser Op
rcv = do
  string "rcv "
  (val:_) <- alnumChars
  optional eol
  return . Rcv $ val

sound :: Parser Op
sound = do
  string "snd "
  val <- alnumChars
  optional eol
  return . Snd $ val

set :: Parser Op
set = do
  string "set "
  [v1:_, v2] <- alnum
  optional eol
  return . Set v1 $ v2

add :: Parser Op
add = do
  string "add "
  [v1:_, v2] <- alnum
  optional eol
  return . Add v1 $ v2

mul :: Parser Op
mul = do
  string "mul "
  [v1:_, v2] <- alnum
  optional eol
  return . Mul v1 $ v2

rmod :: Parser Op
rmod = do
  string "mod "
  [v1:_, v2] <- alnum
  optional eol
  return . Mod v1 $ v2

jgz :: Parser Op
jgz = do
  string "jgz "
  [v1, v2] <- alnum
  return . Jgz v1 $ v2

execute1 :: Computer -> Int
execute1 computer
  | isNothing safeInst = error (show computer)
  | isRcv inst && hasRcv inst = rcv
  | otherwise = execute1 . execute inst $ computer
  where
    safeInst = program computer !? pointer computer
    inst = fromJust safeInst
    isRcv (Rcv v) = True
    isRcv _       = False
    (_ :|> rcv) = send computer
    hasRcv (Rcv v) = (> 0) . findWithDefault 0 v . memory $ computer

execute2 :: Computer -> Computer
execute2 computer
  | isNothing safeInst = error (show computer)
  | isRcv inst && S.null (received computer) = computer
  | isRcv inst = execute2 receive
  | otherwise = execute2 . execute inst $ computer
  where
    safeInst = program computer !? pointer computer
    inst = fromJust safeInst
    isRcv (Rcv v) = True
    isRcv _       = False
    receive =
      computer
        {pointer = pointer computer + 1, received = received', memory = memory'}
    (Rcv r) = inst
    (next :<| received') = received computer
    memory' = insert r next . memory $ computer

parallel :: (Computer, Computer) -> Int
parallel (c0, c1)
  | S.null (send c0) && S.null (send c1) = counter c1
  | otherwise = parallel (c0', c1')
  where
    c0' = execute2 $ c0 {send = S.empty, received = send c1}
    c1' =
      execute2
        $ c1
            { counter = counter c1 + S.length (send c1)
            , send = S.empty
            , received = send c0
            }

execute :: Op -> Computer -> Computer
execute inst computer = computer' {pointer = pointer'}
  where
    pointer' = posInst inst
    computer' = operate inst computer
    posInst (Jgz v1 v2)
      | val v1 computer > 0 = pointer computer + val v2 computer
    posInst _ = pointer computer + 1
    isRcv (Rcv v)
      | (/= 0) . findWithDefault 0 v . memory $ computer = True
    isRcv _ = False
    (_ :|> rcv) = send computer

val :: String -> Computer -> Int
val v computer
  | all isAlpha v = findWithDefault 0 (head v) . memory $ computer
  | otherwise = read v

operate :: Op -> Computer -> Computer
operate (Jgz _ _) computer = computer
operate (Snd v1) computer = computer {send = send computer :|> val v1 computer}
operate (Set v1 v2) computer =
  computer {memory = insert v1 (val v2 computer) . memory $ computer}
operate (Add v1 v2) computer =
  computer {memory = adjust (+ val v2 computer) v1 . memory $ computer}
operate (Mul v1 v2) computer =
  computer {memory = adjust (* val v2 computer) v1 . memory $ computer}
operate (Mod v1 v2) computer =
  computer {memory = adjust (`mod` val v2 computer) v1 . memory $ computer}

part1 :: Bool -> String -> String
part1 _ =
  show
    . execute1
    . Computer 0 S.empty S.empty 0 M.empty
    . fromList
    . parseByLine parseOp

part2 :: Bool -> String -> String
part2 _ input = show . parallel $ (c0, c1)
  where
    program = fromList . parseByLine parseOp $ input
    c0 = execute2 $ Computer 0 S.empty S.empty 0 M.empty program
    c1 = execute2 $ Computer 0 S.empty S.empty 0 (singleton 'p' 1) program
