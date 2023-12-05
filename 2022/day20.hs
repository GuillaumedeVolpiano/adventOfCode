import           General            (preciseTimeIt)
import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.List          as L (take)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)), cycleTaking,
                                           drop, elemIndexL, fromList, index,
                                           length, null, spanl, take, (><))

import           Debug.Trace

decryptionKey = 811589153

index1 = 1000

index2 = 2000

index3 = 3000

parseInput :: [String] -> Seq Int
parseInput = fromList . map read

-- move number i by i positions in the list
move :: Seq Int -> Seq Int
move (a :<| rs) = (Sq.take newIndex rs :|> a) >< Sq.drop newIndex rs
  where
    size = Sq.length rs
    newIndex = mod a size

-- mix the numbers once. We need to call the sequence twice, once as the
-- decreasing sequence for the recurrence, the other as the referrence being
-- mixed.
mix :: Seq Int -> Seq Int
mix numbers = mixed numbers numbers

-- the actual mixing mechanic. We move each number in the sequence in the order
-- they appear in the initial sequence.
mixed :: Seq Int -> Seq Int -> Seq Int
mixed remainingNumbers mixedSequence
  | Sq.null remainingNumbers = mixedSequence
  | otherwise = mixed rest . move $ rotateTo f mixedSequence
  where
    (f :<| rest) = remainingNumbers

-- cycle the sequence until f is at index 0
rotateTo :: Int -> Seq Int -> Seq Int
rotateTo f s
  | elemIndexL f s == Just 0 = s
  | otherwise = after >< before
  where
    (before, after) = spanl (/= f) s

-- calculate the score of the sequence as the sum of the elements at the
-- relevant indexes.
score :: Seq Int -> Int
score s = index r index1 + index r index2 + index r index3
  where
    size = Sq.length s
    r = cycleTaking (index3 + 1) . rotateTo 0 $ s

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  let file = parseInput . lines $ input
      decryptedFile = fmap (decryptionKey *) file
  putStrLn "part 1"
  preciseTimeIt 3 . print . score . mix $ file
  putStrLn "part 2"
  preciseTimeIt 3 . print . score . last . L.take 11 $
    iterate (mixed decryptedFile) decryptedFile
