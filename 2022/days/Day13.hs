module Day13 (part1, part2) where
import           Data.List.Split    (chunksOf, splitOn)

import           Data.Char          (isDigit)
import           Data.List          (sortBy)

data Tree
  = Node [Tree]
  | Leaf Int
  deriving (Show, Eq)

type State = (Packet, Tree)

type Packet = String

day = 13

testOrder :: Tree -> Tree -> Int
testOrder (Leaf a) (Leaf b)
  | a < b = 1
  | b < a = 0
  | a == b = -1
testOrder (Leaf a) (Node b) = testOrder (Node [Leaf a]) (Node b)
testOrder (Node a) (Leaf b) = testOrder (Node a) (Node [Leaf b])
testOrder (Node a) (Node b)
  | null a && not (null b) = 1
  | null a && null b = -1
  | null b && not (null a) = 0
  | testOrder (head a) (head b) == -1 =
    testOrder (Node . tail $ a) (Node . tail $ b)
  | otherwise = testOrder (head a) (head b)

comparePackets :: (Tree, Tree) -> Bool
comparePackets (a, b) = testOrder a b == 1

orderPackets :: Tree -> Tree -> Ordering
orderPackets a b
  | testOrder a b == 1 = LT
  | otherwise = GT

parsePacket :: String -> State
parsePacket s
  | take 2 s == "[]" = (drop 2 s, Node [])
  | isDigit . head $ s = (after, Leaf . read $ before)
  | head s == ',' = parsePacket . tail $ s
  | head s == '[' = parse (tail s) []
  where
    (before, after) = span isDigit s
    parse (']':xs) list = (xs, Node list)
    parse s list =
      let (toParse, parsed) = parsePacket s
       in parse toParse (list ++ [parsed])


packetPairs =map (\(x:y:_) -> (snd . parsePacket $ x, snd . parsePacket $ y)) .
        chunksOf 3 . lines

part1 :: Bool -> String -> String
part1 _ input = show . sum . map (+ 1) . filter (comparePackets . (!!) pp) . take (length pp) $ [0..]
  where
    pp = packetPairs input 

part2 :: Bool -> String -> String
part2 _ input = show . 
    product .
    map (+ 1) .
    filter
      (\x -> (sorted !! x) `elem` [Node [Node [Leaf 2]], Node [Node [Leaf 6]]]) .
    take (length sorted) $
    [0 ..]
      where
        toSort =
          map
            (snd . parsePacket)
            (filter (/= "") ("[[2]]" : "[[6]]" : lines input))
        sorted = sortBy orderPackets toSort
