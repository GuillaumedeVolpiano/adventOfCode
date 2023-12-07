module Day21 (part1, part2) where
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Map           as M (Map, fromList, lookup, update)
import           Data.Maybe         (fromJust, isNothing)

data Monkey
  = LeafMonkey Double
  | OpMonkey String (Double -> Double -> Double) String

type Tree = Map String Monkey

isLeaf :: Monkey -> Bool
isLeaf (LeafMonkey _) = True
isLeaf OpMonkey {}    = False

fromLeaf :: Monkey -> Double
fromLeaf (LeafMonkey a) = a
fromLeaf _              = error "fromLeaf should only be used on Leaf Monkeys"

expand :: Tree -> Monkey -> Double
expand tree (OpMonkey monkey1 op monkey2) =
  op (calculate tree monkey1) (calculate tree monkey2)
expand _ _ = error "expand should only be used on Op Monkeys"

parseLine :: String -> (String, Monkey)
parseLine s = (name, monkey)
  where
    (name, _, rest) = (s =~ ": ") :: (String, String, String)
    monkey
      | (rest =~ "[0-9]+") :: Bool = LeafMonkey $ read (rest =~ "[0-9]+")
      | otherwise = OpMonkey monkey1 op monkey2
    (monkey1, rawOp, monkey2) = (rest =~ " [*+-/] ") :: (String, String, String)
    op
      | rawOp == " / " = (/)
      | rawOp == " * " = (*)
      | rawOp == " + " = (+)
      | rawOp == " - " = (-)

calculate :: Tree -> String -> Double
calculate tree name
  | isNothing (M.lookup name tree) = error (name ++ " is undefined")
  | isLeaf . fromJust . M.lookup name $ tree =
    fromLeaf . fromJust . M.lookup name $ tree
  | otherwise = expand tree . fromJust . M.lookup name $ tree

calculateTwoTrees :: Tree -> String -> Double
calculateTwoTrees tree name = calculate tree monkey2 - calculate tree monkey1
  where
    Just (OpMonkey monkey1 _ monkey2) = M.lookup name tree

alterHuman :: Tree -> Double -> Double
alterHuman tree val =
  calculateTwoTrees (update (\_ -> Just (LeafMonkey val)) "humn" tree) "root"

findHuman :: Tree -> (Double -> Bool) -> Double -> Double -> Double -> Double
findHuman tree comp val1 val2 val3
  | comp $ alterHuman tree val3 = findHuman tree comp val3 (2 * val3) (4 * val3)
  | alterHuman tree val3 == 0 = val3
  | comp $ alterHuman tree val2 = findHuman tree comp val2 (mid val2 val3) val3
  | alterHuman tree val2 == 0 = val2
  | alterHuman tree val1 == 0 = val1
  | comp $ alterHuman tree val1 = findHuman tree comp val1 (mid val1 val2) val2
  | otherwise = error "error, we went too far"
  where
    mid a b = fromIntegral (div (round a + round b) 2) :: Double

-- The use of div brings in rounding errors. We need to check if a lower value
-- can also satisfy the equation.
findLowest :: Tree -> Double -> Int
findLowest tree val
  | alterHuman tree val /= 0 =
    error ("This value is not a solution, even with rounding. " ++ show val)
  | alterHuman tree (val - 1) == 0 = findLowest tree (val - 1)
  | otherwise = round val

part1 :: Bool -> String -> String
part1 _ input = show . round . calculate tree $ "root"
  where
    tree = fromList . map parseLine . lines $ input

part2 :: Bool -> String -> String
part2 _ input = show . round . findHuman tree comp startHuman (2 * startHuman) $
    (4 * startHuman)
  where
    tree = fromList . map parseLine . lines $ input
    startHuman = fromLeaf . fromJust . M.lookup "humn" $ tree
    initialTrees = calculateTwoTrees tree "root"
    comp
      | initialTrees > 0 = (> 0)
      | otherwise = (< 0)
