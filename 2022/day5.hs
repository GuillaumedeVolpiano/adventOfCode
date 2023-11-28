import           Data.List.Split    (chunksOf, splitOn)
import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

import           Data.Char          (digitToInt)
import           Data.IntMap        as I (IntMap, elems, fromList, insert, (!))
import           Data.List          (transpose)
import           Data.Sequence      as S (Seq ((:<|)), fromList, splitAt, (><))

day = 5

newPileFromString :: IntMap String -> String -> IntMap String
newPileFromString pile orders = insert from moved . insert to piled $ pile
  where
    parsedOrders = words orders
    count = read $ parsedOrders !! 1
    from = read $ parsedOrders !! 3
    to = read $ parsedOrders !! 5
    (moved, piled) = move count (pile ! from, pile ! to)
    move 0 (f, t)    = (f, t)
    move x (f:fs, t) = move (x - 1) (fs, f : t)

newPileFromSeq :: IntMap (Seq Char) -> String -> IntMap (Seq Char)
newPileFromSeq pile orders = insert from moved . insert to piled $ pile
  where
    parsedOrders = words orders
    count = read $ parsedOrders !! 1
    from = read $ parsedOrders !! 3
    to = read $ parsedOrders !! 5
    (moving, moved) = S.splitAt count $ pile ! from
    piled = moving >< pile ! to

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let (crates:movements:_) = map lines . splitOn "\n\n" $ input
      initialState =
        map (filter (/= ' ')) . transpose . map (map (!! 1) . chunksOf 4) $
        crates
      initialStateString =
        I.fromList . map (\x -> (digitToInt . last $ x, init x)) $ initialState
      movedPile = foldl newPileFromString initialStateString movements
      initialStateSeq =
        I.fromList . map (\x -> (digitToInt . last $ x, S.fromList . init $ x)) $
        initialState
      movedPileSeq = foldl newPileFromSeq initialStateSeq movements
  putStrLn "part 1"
  print . map head . elems $ movedPile
  putStrLn "part 2"
  print . map (\(a :<| _) -> a) . elems $ movedPileSeq
