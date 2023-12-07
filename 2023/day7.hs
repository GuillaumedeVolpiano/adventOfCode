import           General            (preciseTimeIt, retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Char          (isDigit)
import           Data.List          (group, sort, sortBy)

import           Debug.Trace

class GenHand a where
  handToList :: a -> [Card]
  handType :: a -> Type
  pieceCompare :: a -> a -> Ordering

data Card
  = Low Int
  | T
  | J
  | Q
  | K
  | A
  deriving (Ord, Eq)

data Type
  = High
  | One
  | Two
  | Three
  | Full
  | Four
  | Five
  deriving (Ord, Eq, Show)

data Hand =
  Hand Card Card Card Card Card
  deriving (Eq, Show)

data NewHand =
  NewHand Card Card Card Card Card
  deriving (Eq, Show)

type Bid = Int

instance Show Card where
  show c
    | isLow c = show . fromLow $ c
    | c == T = "T"
    | c == J = "J"
    | c == Q = "Q"
    | c == K = "K"
    | c == A = "A"

instance Ord Hand where
  compare a b
    | handType a > handType b = GT
    | handType a < handType b = LT
    | otherwise = pieceCompare a b

instance Ord NewHand where
  compare a b
    | handType a > handType b = GT
    | handType a < handType b = LT
    | otherwise = pieceCompare a b

instance GenHand Hand where
  handToList :: Hand -> [Card]
  handToList (Hand a b c d e) = [a, b, c, d, e]
  handType :: Hand -> Type
  handType h
    | maxGroup == 5 = Five
    | maxGroup == 4 = Four
    | maxGroup == 1 = High
    | maxGroup == 3 && elem 2 handGroup = Full
    | maxGroup == 3 = Three
    | sort handGroup == [1, 2, 2] = Two
    | otherwise = One
    where
      handGroup = grouped . handToList $ h
      maxGroup = maximum handGroup
  pieceCompare :: Hand -> Hand -> Ordering
  pieceCompare a b = subCompare (handToList a) (handToList b)
    where
      subCompare [] [] = EQ
      subCompare (f:fs) (s:ss)
        | f == s = subCompare fs ss
        | otherwise = compare f s

instance GenHand NewHand where
  handToList :: NewHand -> [Card]
  handToList (NewHand a b c d e) = [a, b, c, d, e]
  handType :: NewHand -> Type
  handType h
    | maxGroup + jsize == 5 = Five
    | maxGroup + jsize == 4 = Four
    | maxGroup + jsize == 3 = testFull
    | maxGroup + jsize == 2 = testTwo
    | maxGroup == 1 = High
    -- there is always a better hand than two pairs if the hands contains a pair
    -- and a joker or no pair and two jokers
    where
      handList = handToList h
      noJoker = filter (/= J) handList
      groupedNoJoker = grouped noJoker
      maxGroup
        | null noJoker = 0
        | otherwise = maximum groupedNoJoker
      jsize = length . filter (== J) $ handList
      -- a Full is either an actual full or two pairs and a joker. With one pair and
      -- two jokers, we get a four.
      testFull
        | jsize == 1 && notElem 1 groupedNoJoker = Full
        | jsize == 0 && elem 2 groupedNoJoker = Full
        | otherwise = Three
      testTwo
        | sort groupedNoJoker == [1, 2, 2] = Two
        | otherwise = One
  pieceCompare :: NewHand -> NewHand -> Ordering
  pieceCompare a b = subCompare (handToList a) (handToList b)
    where
      subCompare [] [] = EQ
      subCompare (f:fs) (s:ss)
        | f == s = subCompare fs ss
        | f == J = LT
        | s == J = GT
        | otherwise = compare f s

parseInput :: String -> (Hand, Bid)
parseInput s = (readHand before, read after)
  where
    (before, _, after) = s =~ " " :: (String, String, String)
    readHand [a, b, c, d, e] =
      Hand (readCard a) (readCard b) (readCard c) (readCard d) (readCard e)
    readCard c
      | isDigit c = Low $ read [c]
      | c == 'T' = T
      | c == 'J' = J
      | c == 'Q' = Q
      | c == 'K' = K
      | c == 'A' = A

fromLow :: Card -> Int
fromLow (Low c) = c
fromLow c       = error ("Low should only be used on low cards. " ++ show c)

isLow :: Card -> Bool
isLow (Low _) = True
isLow _       = False

grouped :: [Card] -> [Int]
grouped = map length . group . sort

handToNew :: Hand -> NewHand
handToNew (Hand a b c d e) = NewHand a b c d e

score :: (Ord a, GenHand a) => [(a, Bid)] -> Int
score =
  sum .
  zipWith (\b c -> b * snd c) [1 ..] . sortBy (\a b -> compare (fst a) (fst b))

pairToNew :: (Hand, Bid) -> (NewHand, Bid)
pairToNew (a, b) = (handToNew a, b)

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  putStrLn "part 1"
  preciseTimeIt 3 . print . score . map parseInput . lines $ input
  putStrLn "part 2"
  preciseTimeIt 3 . print . score . map (pairToNew . parseInput) . lines $ input
