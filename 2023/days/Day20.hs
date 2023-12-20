{-# LANGUAGE TupleSections #-}

module Day20
  ( part1
  , part2
  ) where

import           Data.List       as L (null)
import           Data.List.Split (splitOn)
import           Data.Map        as M (Map, elems, empty, fromList, insert,
                                       lookup, member, null, (!))
import           Data.Maybe      (Maybe (Just, Nothing), isNothing)
import           Data.Sequence   as Sq (Seq ((:<|)), empty, fromList, null,
                                        singleton, (><))
import           Helpers.Graph   (dicToGraph, graphToViz)
import           Linear.V2       (V2 (..))

data Module
  = Broadcaster
  | Button
  | FlipFlop Bool
  | Conjunction (Map String Pulse)
  | Accumulator [Pulse]
  | AccumConjunction (Map String Pulse) [String]
  deriving (Show, Eq)

data Pulse
  = L
  | H
  deriving (Show, Eq)

type Circuit = Map String (Module, [String])

type Action = (Pulse, From, To)

type Acc = V2 Int

type From = String

type To = String

type State = (Seq Action, Acc, Circuit)

emptyAcc = V2 0 0

finalMod = "rx"

accMod = "cs"

eff :: Pulse -> Acc
eff L = V2 1 0
eff H = V2 0 1

receiveImpulse :: State -> State
receiveImpulse state@(actions, _, _)
  | Sq.null actions = state
receiveImpulse ((p, _, t) :<| actions, acc, circuit)
  | not (member t circuit) = receiveImpulse (actions, acc + eff p, circuit)
receiveImpulse ((p, f, t) :<| actions, acc, circuit) =
  receiveImpulse
    (actions >< newAct, acc + eff p, insert t (newM, dests) circuit)
  where
    cur@(_, dests) = circuit ! t
    (newM, newAct) = act p f t cur

act :: Pulse -> From -> To -> (Module, [String]) -> (Module, Seq Action)
act p _ _ (Accumulator acc, _) = (Accumulator (p : acc), Sq.empty)
act p _ t (Broadcaster, dest) = (Broadcaster, newSeq p t dest)
act _ _ t (Button, dest) = (Button, newSeq L t dest)
act p _ t (FlipFlop b, dest)
  | p == H = (FlipFlop b, Sq.empty)
  | b = (FlipFlop False, newSeq L t dest)
  | otherwise = (FlipFlop True, newSeq H t dest)
act p f t (Conjunction dic, dest)
  | all (== H) . elems $ newDic = (Conjunction newDic, newSeq L t dest)
  | otherwise = (Conjunction newDic, newSeq H t dest)
  where
    newDic = insert f p dic
act p f t (AccumConjunction dic l, dest)
  | all (== H) . elems $ newDic =
    (AccumConjunction newDic newL, newSeq L t dest)
  | otherwise = (AccumConjunction newDic newL, newSeq H t dest)
  where
    newDic = insert f p dic
    newL
      | p == H = f : l
      | otherwise = l

newSeq :: Pulse -> From -> [To] -> Seq Action
newSeq p f = Sq.fromList . map (p, f, )

toState :: [[String]] -> State
toState input =
  (singleton (L, "button", "broadcaster"), emptyAcc, ) .
  insert accMod (AccumConjunction dicCx [], destCx) $
  dic
  where
    split = map (\[a, b] -> (a, splitOn ", " b)) input
    dic =
      M.fromList .
      (:) (finalMod, (Accumulator [], [])) .
      map
        ((\((a, b), c) -> (a, (b, c))) . (\(a, b) -> (nameToModule a split, b))) $
      split
    (Conjunction dicCx, destCx) = dic ! accMod

pressButton :: State -> State
pressButton = receiveImpulse . emptyState

emptyState :: State -> State
emptyState (_, acc, b) =
  ( singleton (L, "button", "broadcaster")
  , acc
  , insert finalMod (Accumulator [], []) .
    insert accMod (AccumConjunction dicCs [], destCs) $
    b)
  where
    (AccumConjunction dicCs _, destCs) = b ! accMod

nameToModule :: String -> [(String, [String])] -> (String, Module)
nameToModule "broadcaster" _ = ("broadcaster", Broadcaster)
nameToModule "button" _ = ("button", Button)
nameToModule ('%':name) _ = (name, FlipFlop False)
nameToModule ('&':name) list = (name, Conjunction dic)
  where
    dic =
      M.fromList . map ((, L) . stripType . fst) . filter (elem name . snd) $
      list
    stripType name
      | head name `elem` "%&" = tail name
      | otherwise = name

pulseProduct :: Acc -> Int
pulseProduct (V2 x y) = x * y

checkAccConjunction :: State -> [String]
checkAccConjunction (_, _, s) = l
  where
    (AccumConjunction _ l, _) = s ! accMod

part1 :: Bool -> String -> String
part1 _ =
  show .
  pulseProduct .
  last .
  map (\(_, v, _) -> v) .
  take 1001 . iterate pressButton . toState . map (splitOn " -> ") . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  product .
  map fst .
  filter (not . L.null . snd) .
  zip [0 .. 5000] .
  map checkAccConjunction .
  take 5001 . iterate pressButton . toState . map (splitOn " -> ") . lines
