module Day16
  ( part1
  , part2
  ) where

import           Data.Sequence as Sq (Seq ((:<|), (:|>)), empty, fromList,
                                      length, null, reverse, take, (><))
import           Data.Text     (Text, unpack)

-- to be continued. The final bits are going to be of the form a · b · a · b · a
-- ·b…
--
-- In the end, there will be k ab pairs, and (2k - 1) separators.
--
-- The total length of the filler will be 2k (length a + 1) - 1, that is 2k length a - 1,
-- which will then need to be trimmed to the actual length of the filler.
-- The size of the chunks will be the highest power of 2 that divide the disk size.
--
-- The parity of the chunk will be determined by the parity of the separators +
-- that of the potential trailing or starting parts of an ab pair.
--
-- The separators form what is called a dragon curve (see hint in input). The
-- nth element of the dragon curve can be found with the formula below
-- Or just use sequences of Bools ?
dragon :: Int -> Seq Bool -> Seq Bool
dragon size filler
  | Sq.length filler >= size = Sq.take size filler
  | otherwise = dragon size $ filler >< (False :<| reverted)
  where
    reverted = fmap not . Sq.reverse $ filler

checkSum :: Seq Bool -> Seq Bool
checkSum filler
  | odd . Sq.length $ filler = filler
  | otherwise = checkSum . reduce $ filler
  where
    reduce s
      | Sq.null s = empty
    reduce (a :<| b :<| rest) = (a == b) :<| reduce rest

part1 :: Bool -> Text -> String
part1 test =
  foldr
    (\x r ->
       if x
         then '1' : r
         else '0' : r)
    ""
    . checkSum
    . dragon size
    . fromList
    . map (== '1')
    . init
    . unpack
  where
    size
      | test = 20
      | otherwise = 272

part2 :: Bool -> Text -> String
part2 _ =
  foldr
    (\x r ->
       if x
         then '1' : r
         else '0' : r)
    ""
    . checkSum
    . dragon 35651584
    . fromList
    . map (== '1')
    . init
    . unpack
