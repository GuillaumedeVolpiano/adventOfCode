{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day11
  ( part1
  , part2
  , easyOut
  ) where

import           Control.Monad                (void)
import           Data.ByteString              (ByteString)
import qualified Data.IntSet                  as IS (insert, member, delete, singleton, findMin, foldr, toList, union, filter, notMember, difference, intersection, null)
import           Data.Word                    (Word8)
import           Data.Word8                   (_colon, _lf, _space)
import qualified Streamly.Data.Fold           as F (foldl', toList)
import           Streamly.Data.Fold           (Fold)
import qualified Streamly.Data.Parser         as P (eof, manyTill, satisfy,
                                                    takeEQ)
import           Streamly.Data.Parser         (Parser)
import qualified Streamly.Data.Stream         as S (parse)
import           Streamly.Data.Stream         (Stream)
import qualified Streamly.External.ByteString as SBS (write)
import qualified Data.HashMap.Strict as HM ((!), lookup, insert)
import qualified Data.List as L (sortBy)
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, length, unsafeIndex, thaw)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM ((!), insert, member, notMember)
import qualified Data.Vector.Mutable as MV (unsafeRead, unsafeWrite, replicate, imapM_, length)
import Data.Vector.Mutable (IOVector)

parsePaths :: Parser Word8 IO (Int, Int, Vector IntSet)
parsePaths = P.manyTill parseNode P.eof vectorise

vectorise :: Fold IO (ByteString, [ByteString]) (Int, Int, Vector IntSet)
vectorise = rmf <$> F.foldl' folder (0, mempty, [])
  where
    rmf (_, nm, v) = (nm HM.! "you", nm HM.! "out", V.fromList . map snd . L.sortBy (comparing fst) $ (nm HM.! "out", mempty):v)
    folder (i, nm, v) (node, edges) = (i'', nm'', (inode, edges'):v)
      where
        (i', nm', inode) = case HM.lookup node nm of
                              Nothing -> (i + 1, HM.insert node i nm, i)
                              Just inode' -> (i, nm, inode')
        (i'', edges', nm'') = foldr subfolder (i', mempty, nm') edges
        subfolder edge (index, es, nodemap) = case HM.lookup edge nm of
                                                Nothing -> (index + 1, IS.insert index es, HM.insert edge index nodemap)
                                                Just inode'' -> (index, IS.insert inode'' es, nodemap)


parseNode :: Parser Word8 IO (ByteString, [ByteString])
parseNode = do
  node <- P.takeEQ 3 SBS.write
  void $ P.satisfy (==_colon)
  (node, ) <$> P.manyTill (P.satisfy (==_space) >> P.takeEQ 3 SBS.write) (P.satisfy (==_lf)) F.toList


calcPaths :: (Int, Int, Vector IntSet) -> IO Int
calcPaths (you, out, graph) = do
  let (_, _, _, _, scc) = foldr (tarjan graph) (0, [], mempty, mempty, []) [0..V.length graph -1]
  tg <- V.thaw graph
  mapM_ (uncycle you tg) scc
  memo <- MV.replicate (MV.length tg) (-1)
  dfs memo tg you
  where
    dfs memo tg v 
      | v == out = pure 1
      | otherwise = do
          r <- MV.unsafeRead memo v
          if r >= 0 then pure r
                    else do
                      sl <- IS.toList <$> MV.unsafeRead tg v
                      result <- sum <$> mapM (dfs memo tg) sl
                      MV.unsafeWrite memo v result
                      pure result

uncycle :: Int -> IOVector IntSet -> IntSet -> IO ()
uncycle you graph set = do
  let nv = if you `IS.member` set then you else IS.findMin set
      sl = IS.toList set
  newOuts <- IS.filter (`IS.notMember` set) . foldr IS.union mempty <$> mapM (MV.unsafeRead graph) sl
  MV.imapM_ (\i s -> if i == nv then pure newOuts else if i `IS.member` set then pure mempty else if IS.null (IS.intersection set s) then pure s else pure . IS.insert nv . IS.difference s $ set) graph





tarjan :: Vector IntSet -> Int
       -> (Int, [Int], IntSet, IntMap Int, [IntSet])
       -> (Int, [Int], IntSet, IntMap Int, [IntSet])
tarjan graph node (index, stack, onStack, indexMap, sccs) = if node `IM.member` indexMap
                                                                 then (index, stack, onStack, indexMap, sccs)
                                                                 else strongConnect'
  where
    strongConnect' = (\(i, s, o, im, sc, _) -> (i, s, o, im, sc)) $ strongConnect graph node index stack onStack indexMap sccs

strongConnect :: Vector IntSet -> Int -> Int -> [Int] -> IntSet
  -> IntMap Int -> [IntSet]
  -> (Int, [Int], IntSet, IntMap Int, [IntSet], Int)
strongConnect graph node index stack onStack indexMap sccs = (index'', newStack, newOn, indexMap'', sccs'', lowlink')
  where
    indexMap' = IM.insert node index indexMap
    lowlink = index
    index' = index + 1
    stack' = node:stack
    onStack' = IS.insert node onStack
    edges = V.unsafeIndex graph node
    (index'', stack'', onStack'', indexMap'', sccs', lowlink') = IS.foldr consider (index', stack', onStack', indexMap', sccs, lowlink) edges
    consider w (i, s, o, im, sc, l)
      | w `IM.notMember` im = (\(a, b, c, d, e, f) -> (a, b, c, d, e, min l f)) . strongConnect graph w i s o im $ sc
      | w `IS.member` o = (i, s, o, im, sc, min l $ im IM.! w)
      | otherwise = (i, s, o, im, sc, l)
    (newStack, newOn, scc) = if lowlink' == index
                             then (\(a, b, c) -> (a, b, Just c)) $ extractScc stack'' onStack''
                             else (stack'', onStack'', Nothing)
    extractScc (w:s) o
      | w == node = (s, IS.delete w o, IS.singleton w)
      | otherwise = (\(a, b, c) -> (a, b, IS.insert w c)) $ extractScc s (IS.delete w o)
    extractScc [] _ = undefined
    sccs'' = maybe sccs' (:sccs') scc


easyOut :: Stream IO Word8 -> IO Int
easyOut s = do
  parsed <- either undefined id <$> S.parse parsePaths s
  calcPaths parsed

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = easyOut s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ _ = print ("Part 2" :: String)
