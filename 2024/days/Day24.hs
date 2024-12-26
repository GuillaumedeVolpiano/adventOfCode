{-# LANGUAGE TupleSections #-}

module Day24
  ( part1
  , part2
  ) where

import           Data.Bifunctor             (bimap, first)
import           Data.Bits                  (bit, shiftL, shiftR, testBit, xor,
                                             (.&.))
import           Data.Char                  (chr, isLower, ord)
import           Data.Either                (fromRight)
import           Data.IntMap                (IntMap, assocs, empty, fromList,
                                             insert, keys, member, notMember,
                                             size, (!))
import           Data.List                  (foldl', intercalate, nub, sort,
                                             sortBy, tails, (\\))
import           Data.Maybe                 (fromJust, isNothing, mapMaybe)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text, pack)
import           Data.Text.Lazy             (unpack)
import           Helpers.Graph              (assocsToGraph, graphToViz)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (eof, manyTill, parse, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Debug.Trace

type Register = IntMap Bool

data Op
  = AND
  | OR
  | XOR
  deriving (Show, Eq, Ord, Read)

newtype Gates =
  Gates (IntMap (Op, Int, Int))
  deriving (Show)

insertGates :: Int -> (Op, Int, Int) -> Gates -> Gates
insertGates key val (Gates gates) = Gates . insert key val $ gates

(!!!) :: Gates -> Int -> (Op, Int, Int)
(!!!) (Gates gates) = (gates !)

notMemberGates :: Int -> Gates -> Bool
notMemberGates key (Gates gates) = key `notMember` gates

memberGates :: Int -> Gates -> Bool
memberGates key (Gates gates) = key `member` gates

parseInput :: Parser (Register, Gates)
parseInput = do
  register <- parseRegister
  gates <- parseGates
  return (register, gates)

parseRegister :: Parser Register
parseRegister =
  parseCell <|> do
    eol
    return empty

parseGates :: Parser Gates
parseGates =
  parseGate <|> do
    eof
    return . Gates $ empty

parseCell :: Parser Register
parseCell = do
  cell <- encode <$> manyTill alphaNumChar (string . pack $ ": ")
  value <- (== 1) <$> decimal
  eol
  insert cell value <$> parseRegister

parseGate :: Parser Gates
parseGate = do
  cell1 <- encode <$> manyTill alphaNumChar (char ' ')
  op <- read <$> manyTill alphaNumChar (char ' ')
  cell2 <- encode <$> manyTill alphaNumChar (string . pack $ " -> ")
  dest <- encode <$> manyTill alphaNumChar eol
  insertGates dest (op, cell1, cell2) <$> parseGates

encode :: String -> Int
encode = foldr (\c -> (+ ord c) . flip shiftL 7) 0

decode :: Int -> String
decode 0 = ""
decode i = chr (i .&. 127) : (decode . shiftR i $ 7)

gateVal :: Gates -> Register -> Int -> Bool
gateVal g@(Gates gates) register key = comp val1 val2
  where
    (op, cell1, cell2) = gates ! key
    val1
      | cell1 `member` register = register ! cell1
      | otherwise = gateVal g register cell1
    val2
      | cell2 `member` register = register ! cell2
      | otherwise = gateVal g register cell2
    comp
      | op == AND = (&&)
      | op == OR = (||)
      | op == XOR = (/=)

findZs :: Gates -> [Int]
findZs (Gates gates) = filter ((== ord 'z') . (.&. 127)) . keys $ gates

toNumber :: [Bool] -> Int
toNumber = foldr bitSum 0
  where
    bitSum v acc
      | v = 1 + shiftL acc 1
      | otherwise = shiftL acc 1

findNumber :: (Register, Gates) -> Int
findNumber (register, gates) =
  toNumber . map (gateVal gates register) . sortBy specialCompare . findZs
    $ gates

specialCompare :: Int -> Int -> Ordering
specialCompare a b
  | a == 0 && b == 0 = EQ
  | a == 0 = GT
  | b == 0 = LT
  | otherwise =
    compare (a .&. 127) (b .&. 127)
      `mappend` specialCompare (shiftR a 7) (shiftR b 7)

testFullNumbers :: Register -> Gates -> Int -> Int -> Bool
testFullNumbers register gates x y =
  x + y == findNumber (fromList $ xs ++ ys, gates)
  where
    bitSize = div (size register) 2
    xs = [(encodeInt 'x' i, testBit x i) | i <- [0 .. bitSize - 1]]
    ys = [(encodeInt 'y' i, testBit y i) | i <- [0 .. bitSize - 1]]

testNumbers :: Bool -> Register -> Gates -> Int -> Int -> Int -> Bool
testNumbers test register gates relevantBit x y =
  testBit (x `op` y) relevantBit
    == testBit (findNumber (fromList $ xs ++ ys, gates)) relevantBit
  where
    bitSize = div (size register) 2
    xs = [(encodeInt 'x' i, testBit x i) | i <- [0 .. bitSize - 1]]
    ys = [(encodeInt 'y' i, testBit y i) | i <- [0 .. bitSize - 1]]
    op
      | test = (.&.)
      | otherwise = (+)

encodeInt :: Char -> Int -> Int
encodeInt c i
  | i < 10 = encode (c : '0' : show i)
  | otherwise = encode (c : show i)

faultyEntries :: Bool -> (Register, Gates) -> [Int]
faultyEntries test pair@(register, _) =
  filter (failedTests test pair) [0 .. bitSize - 1]
  where
    bitSize = div (size register) 2

passedCarry :: Register -> Gates -> Bool
passedCarry register gates =
  all (passedCarryTests register gates) [2 .. bitSize - 1]
  where
    bitSize = div (size register) 2

passedCarryTests :: Register -> Gates -> Int -> Bool
passedCarryTests register gates k =
  all (uncurry (testFullNumbers register gates))
    $ [ (a + b + c, d + e + f)
      | a <- [0, bit k]
      , b <- [0, bit (k - 1)]
      , c <- [0, bit (k - 1) - 1]
      , a /= 0 || b /= 0 || c /= 0
      , d <- [0, bit k]
      , e <- [0, bit (k - 1)]
      , f <- [0, bit (k - 1) - 1]
      , d /= 0 || e /= 0 || f /= 0
      ]

failedTests :: Bool -> (Register, Gates) -> Int -> Bool
failedTests test (register, gates) k =
  not . all (uncurry (testNumbers test register gates k))
    $ [(x, y) | x <- [0, bit k], y <- [0, bit k]]

ancestors :: Gates -> Int -> [Int]
ancestors g@(Gates gates) int
  | int `notMember` gates = []
  | cell1 `notMember` gates && cell2 `notMember` gates = []
  | cell1 `notMember` gates = cell2 : ancestors g cell2
  | cell2 `notMember` gates = cell1 : ancestors g cell1
  | otherwise = cell1 : cell2 : concatMap (ancestors g) [cell1, cell2]
  where
    (_, cell1, cell2) = gates ! int

swappable :: Gates -> Int -> [Int]
swappable gates cell
  | cell .&. 128 == 0 && cell .&. 16384 == 0 = ancestors gates cell
  | otherwise = ancestors gates cell \\ ancestors gates before
  where
    before = pre cell

pre :: Int -> Int
pre = encodeInt 'z' . (+ (-1)) . read . tail . decode

suc :: Int -> Int
suc = encodeInt 'z' . (+ 1) . read . tail . decode

searchFix :: (Register, Gates) -> [String]
searchFix pair@(register, gates) =
  map (intercalate "," . sort . fst)
    . filter (passedCarry register . snd)
    . foldl' concatFix [([], gates)]
    . faultyEntries False
    $ pair
  where
    concatFix prev bit =
      map (\(s, ((a, b), g)) -> (decode a : decode b : s, g))
        . concatMap
            (\p ->
               map (fst p, ) . filter (not . null) . fixOne (register, snd p)
                 $ bit)
        $ prev

invert :: Gates -> (Int, Int) -> Gates
invert gates (a, b) =
  insertGates a (gates !!! b) . insertGates b (gates !!! a) $ gates

fixOne :: (Register, Gates) -> Int -> [((Int, Int), Gates)]
fixOne (register, gates) bit =
  map (\p -> (p, invert p)) . filter passTest $ pairs
  where
    zed = encodeInt 'z' bit
    (Gates inGates) = gates
    pairs =
      nub
        . concatMap (filter notImmediateAncestor)
        . zipWith (\a -> map (a, )) toTest
        . tail
        . tails
        $ toTest
    invert (a, b) =
      insertGates a (gates !!! b) . insertGates b (gates !!! a) $ gates
    passTest pair =
      not . any (failedTests False (register, invert pair))
        $ [bit, bit - 1, bit + 1]
    pass = filter passTest pairs
    notImmediateAncestor (a, b) =
      a /= b
        && a `memberGates` gates
        && b `memberGates` gates
        && a `notElem` ancestors gates b
        && b `notElem` ancestors gates a
    toTest
      | bit == 0 = zed : swappable gates zed ++ swappable gates (suc zed)
      | otherwise =
        zed
          : swappable gates zed
          ++ swappable gates (pre zed)
          ++ swappable gates (suc zed)

part1 :: Bool -> Text -> String
part1 _ =
  show
    . findNumber
    . fromRight (error "parser error")
    . parse parseInput "day24"

graph :: Gates -> String
graph (Gates gates) =
  unpack
    . graphToViz
    . assocsToGraph
    . map
        (\(a, (op, b, c)) ->
           (decode a, [(decode b, show op), (decode c, show op)]))
    . assocs
    $ gates

part2 :: Bool -> Text -> String
part2 _ =
  unlines
    . searchFix
    . fromRight (error "parser error")
    . parse parseInput "day24"
