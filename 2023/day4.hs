import           Data.Maybe         (fromJust)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)), adjust,
                                           replicate, splitAt, (!?), (><))
import           Data.Set           as St (Set, fromList, intersection, size)
import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

data Card =
  Card Index Winning Have

type Index = Int

type Winning = Set Int

type Have = Set Int

parseLine :: String -> Card
parseLine s = Card index winning have
  where
    (before, _, after) = s =~ "\\|" :: (String, String, String)
    (card, _, rawWinning) = before =~ ":" :: (String, String, String)
    index = read (card =~ "[0-9]+")
    winning = fromList . map read $ getAllTextMatches (rawWinning =~ "[0-9]+")
    have = fromList . map read $ getAllTextMatches (after =~ "[0-9]+")

scoreCard :: Card -> Int
scoreCard (Card _ winning have)
  | inter == 0 = 0
  | otherwise = 2 ^ (inter - 1)
  where
    inter = size . intersection have $ winning

totCards :: Seq Int -> [Card] -> Int
totCards numCards [] = sum numCards
totCards numCards (card@(Card index winning have):cs) = totCards newNumCards cs
  where
    totCard = fromJust $ numCards !? (index - 1)
    score = size . intersection have $ winning
    newNumCards
      | score == 0 = numCards
      | otherwise =
        foldr (adjust (+ totCard)) numCards [index .. (index + score - 1)]

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  let cards = map parseLine . lines $ input
  putStrLn "part 1"
  print . sum . map scoreCard $ cards
  putStrLn "part 2"
  print $ totCards (Sq.replicate (length cards) 1) cards
