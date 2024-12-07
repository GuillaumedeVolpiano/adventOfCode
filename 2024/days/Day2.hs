module Day2
  ( part1
  , part2
  ) where

import           Data.Ix              (inRange)
import           Data.List            as L (length)
import           Data.Sequence        as Sq (Seq ((:<|), (:|>)), empty,
                                             fromList, length, null, splitAt,
                                             (><))
import           Data.Sequences       (IsSequence, tailEx)
import           Data.Text            as T (Text)
import           Data.Zip             as Z (Zip, zipWith)
import           Helpers.Parsers.Text (signedInts)

isSafe :: Foldable t => t Int -> Bool
isSafe diffs = all (inRange (1, 3)) diffs || all (inRange (-3, -1)) diffs

dampenedSafe :: Seq Int -> Bool
dampenedSafe levels = any (dampen levels) [0 .. Sq.length levels - 1]

dampen :: Seq Int -> Int -> Bool
dampen levels index = checkSafe (xs >< ys)
  where
    (xs, _ :<| ys) = Sq.splitAt index levels

checkSafe :: (Foldable t, Zip t, IsSequence (t Int)) => t Int -> Bool
checkSafe = isSafe . (Z.zipWith (-) <*> tailEx)

part1 :: Bool -> Text -> String
part1 _ = show . L.length . filter checkSafe . signedInts

part2 :: Bool -> Text -> String
part2 _ = show . L.length . filter dampenedSafe . map fromList . signedInts
