import           General            (preciseTimeIt, retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Char          (isDigit)
import           Data.List          (group, sort, sortBy)

import           Debug.Trace

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

type Hand = [Card]

type Bid = Int

instance Show Card where
  show c
    | isLow c = show . fromLow $ c
    | c == T = "T"
    | c == J = "J"
    | c == Q = "Q"
    | c == K = "K"
    | c == A = "A"

parseInput :: String -> (Hand, Bid)
parseInput s = (readHand before, read after)
  where
    (before, _, after) = s =~ " " :: (String, String, String)
    readHand []     = []
    readHand (c:cs) = readCard c : readHand cs
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

handType :: Hand -> Type
handType h
  | maxGroup == 5 = Five
  | maxGroup == 4 = Four
  | maxGroup == 1 = High
  | maxGroup == 3 && elem 2 (grouped h) = Full
  | maxGroup == 3 = Three
  | sort (grouped h) == [1, 2, 2] = Two
  | otherwise = One
  where
    maxGroup = maximum . grouped $ h

newType :: Hand -> Type
newType h
  | maxGroup + jsize == 5 = Five
  | maxGroup + jsize == 4 = Four
  | maxGroup + jsize == 3 = testFull
  | maxGroup + jsize == 2 = testTwo
  | maxGroup == 1 = High
  -- there is always a better hand than two pairs if the hands contains a pair
  -- and a joker or no pair and two jokers
  where
    noJoker = filter (/= J) h
    groupedNoJoker = grouped noJoker
    maxGroup
      | null noJoker = 0
      | otherwise = maximum groupedNoJoker
    jsize = length . filter (== J) $ h
  -- a Full is either an actual full or two pairs and a joker. With one pair and
  -- two jokers, we get a four.
    testFull
      | jsize == 1 && notElem 1 groupedNoJoker = Full
      | jsize == 0 && elem 2 groupedNoJoker = Full
      | otherwise = Three
    testTwo
      | sort groupedNoJoker == [1, 2, 2] = Two
      | otherwise = One

grouped :: Hand -> [Int]
grouped = map length . group . sort

compareHands :: (Hand, Int) -> (Hand, Int) -> Ordering
compareHands (a, _) (b, _)
  | handType a > handType b = GT
  | handType a < handType b = LT
  | otherwise = pieceCompare a b
  where
    pieceCompare [] [] = EQ
    pieceCompare (f:fs) (s:ss)
      | f == s = pieceCompare fs ss
      | otherwise = compare f s

newCompare :: (Hand, Int) -> (Hand, Int) -> Ordering
newCompare (a, _) (b, _)
  | newType a > newType b = GT
  | newType a < newType b = LT
  | otherwise = pieceCompare a b
  where
    pieceCompare [] [] = EQ
    pieceCompare (f:fs) (s:ss)
      | f == s = pieceCompare fs ss
      | f == J = LT
      | s == J = GT
      | otherwise = compare f s

score :: [(Hand, Int)] -> Int
score = sum . zipWith (\b c -> b * snd c) [1 ..]

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  putStrLn "part 1"
  preciseTimeIt 3 . print . score . sortBy compareHands . map parseInput . lines $
    input
  putStrLn "part 2"
  preciseTimeIt 3 . print . score . sortBy newCompare . map parseInput . lines $
    input
