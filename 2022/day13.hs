import           Data.List.Split    (chunksOf, splitOn)
import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

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

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let packetPairs =
        map (\(x:y:_) -> (snd . parsePacket $ x, snd . parsePacket $ y)) .
        chunksOf 3 . lines $
        input
      toSort =
        map
          (snd . parsePacket)
          (filter (/= "") ("[[2]]" : "[[6]]" : lines input))
      sorted = sortBy orderPackets toSort
  putStrLn "part 1"
  print .
    sum .
    map (+ 1) .
    filter (comparePackets . (!!) packetPairs) . take (length packetPairs) $
    [0 ..]
  putStrLn "part 2"
  print .
    product .
    map (+ 1) .
    filter
      (\x -> (sorted !! x) `elem` [Node [Node [Leaf 2]], Node [Node [Leaf 6]]]) .
    take (length sorted) $
    [0 ..]
