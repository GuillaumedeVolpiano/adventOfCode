module Day19
  ( part1
  , part2
  ) where

import           Data.List.Split (splitWhen)
import           Data.Map        as M (Map, fromList, (!))
import           Data.Maybe      (Maybe (Just, Nothing), catMaybes, fromJust,
                                  isNothing)
import           Part            (Accepted, Condition, Part, Range, System,
                                  Workflow, empty, parseInput, size)
import           Text.Regex.TDFA (getAllTextMatches, (=~))

-- Main body
processAll :: Accepted a -> (System a, [Part a]) -> Accepted a
processAll proc (system, []) = proc
processAll proc (system, p:ps) =
  processAll (process proc system p "in") (system, ps)

process :: Accepted a -> System a -> Part a -> Workflow -> Accepted a
process accepted system part workflow = finalAccepted
  where
    processes = system ! workflow
    processProcesses (ar, pr, tp) p =
      let (na, processed, unprocessed) = processOne ar tp p
       in (na, processed : pr, unprocessed)
    (intAccepted, rawToProcess, _) =
      foldl processProcesses (accepted, [], Just part) processes
    stillToProcess = catMaybes rawToProcess
    finalAccepted =
      foldl (\a b -> uncurry (process a system) b) intAccepted stillToProcess

processOne ::
     Accepted a
  -> Maybe (Part a)
  -> (Condition a, Workflow)
  -> (Accepted a, Maybe (Part a, Workflow), Maybe (Part a))
processOne accepted Nothing _ = (accepted, Nothing, Nothing)
processOne accepted (Just part) (condition, workflow)
  | empty processed || workflow == "R" =
    (accepted, Nothing, testEmpty unprocessed)
  | workflow == "A" = (processed : accepted, Nothing, testEmpty unprocessed)
  | otherwise = (accepted, Just (processed, workflow), testEmpty unprocessed)
  where
    (processed, unprocessed) = condition part
    testEmpty pr
      | empty pr = Nothing
      | otherwise = Just pr

part1 :: Bool -> String -> String
part1 _ input =
  show . sum . map size . processAll [] $
  (parseInput input :: (System Int, [Part Int]))

part2 :: Bool -> String -> String
part2 _ input =
  show . sum . map size . processAll [] $
  (parseInput input :: (System Range, [Part Range]))
