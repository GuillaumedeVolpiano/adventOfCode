module Main where

import           Data.Ix              (inRange)
import           Data.List            (inits)
import           Data.Map             as M (empty)
import           Data.Maybe           (isJust)
import           Data.Set             as S (Set, empty, fromList, member,
                                            notMember)
import           Data.Text            as T (Text)
import qualified Data.Text.IO.Utf8    as TIO (readFile)
import           Helpers.Graph        (Pos, dirs, origin)
import           Helpers.Parsers.Text (signedInts)
import           Helpers.Search       (bfsSafeDist, dfs, listBfsSafe)
import           Linear.V2            (V2 (..))
import           System.Directory     (getHomeDirectory)
import           Test.Tasty.Bench     (Benchmark, bcompare, bench, bgroup,
                                       defaultMain, nf)

import           Debug.Trace

inputPath = "/adventOfCode/input/2024/day18.txt"

goal = V2 70 70

range = (origin, goal)

bfsListBench :: Set Pos -> Bool
bfsListBench walls =
  isJust . listBfsSafe [origin] 0 M.empty (neighbours walls) $ (== goal)

bfsBench :: Set Pos -> Bool
bfsBench walls = isJust . bfsSafeDist origin (neighbours walls) $ (== goal)

dfsBench :: Set Pos -> Bool
dfsBench walls = member goal . dfs [origin] (neighbours walls) $ S.empty

neighbours :: Set Pos -> Pos -> [Pos]
neighbours walls pos =
  filter (\p -> inRange range p && p `S.notMember` walls) . map (pos +) $ dirs

tests :: Text -> [Benchmark]
tests input = [bfss, bfslists, dfss]
  where
    samples =
      (\a -> map (\b -> fromList . take (2 ^ b) $ a) [1 .. 12])
        . map (\[a, b] -> V2 a b)
        . signedInts
        $ input :: [Set Pos]
    bfss =
      bgroup "BFS" . map (\x -> bench (show . length $ x) . nf bfsBench $ x)
        $ samples
    bfslists =
      bgroup "BFSList"
        . map (\x -> bench (show . length $ x) . nf bfsListBench $ x)
        $ samples
    dfss =
      bgroup "DFS" . map (\x -> bench (show . length $ x) . nf dfsBench $ x)
        $ samples

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- TIO.readFile $ home ++ inputPath
  defaultMain $ tests file
