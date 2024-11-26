{-# LANGUAGE TupleSections #-}

module Day21
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (second)
import           Data.List            as L (groupBy, reverse, sortBy, transpose,
                                            (\\))
import           Data.Map             as M (Map, filterWithKey, fromList,
                                            mapWithKey, member, (!))
import           Data.Ord             (comparing)
import           Helpers.Parsers      (Parser, parseByLine)
import           Text.Megaparsec      (many, optional, parse, sepBy, (<|>))
import           Text.Megaparsec.Char (char, eol, printChar, string)

type Shape = [String]

type SimpleTranslator = Map Shape Shape

type TripleTranslator = Map Shape [Shape]

initialState = [".#.", "..#", "###"]

round1 :: SimpleTranslator -> Shape -> [Shape]
round1 st three = map (transform st) [ul, ur, ll, lr]
  where
    [[a, b, c, d], [e, f, g, h], [i, j, k, l], [m, n, o, p]] = st ! three
    ul = [[a, b], [e, f]]
    ur = [[c, d], [g, h]]
    ll = [[i, j], [m, n]]
    lr = [[k, l], [o, p]]

round2 :: SimpleTranslator -> Shape -> [Shape]
round2 st three = map (transform st) [ul', uc, ur', cl, c, cr, ll', lc, lr']
  where
    [ul, ur, ll, lr] = map (st !) . round1 st $ three
    ul' = map init . init $ ul
    ur' = map tail . init $ ur
    ll' = map init . tail $ ll
    lr' = map tail . tail $ lr
    central [a, b] [c, d] = [[a, c], [b, d]]
    uc = central (map last . init $ ul) (map head . init $ ur)
    lc = central (map last . tail $ ll) (map head . tail $ lr)
    cl = [init . last $ ul, init . head $ ll]
    cr = [tail . last $ ur, tail . head $ lr]
    c =
      [ [last . last $ ul, head . last $ ur]
      , [last . head $ ll, head . head $ lr]
      ]

round3 :: SimpleTranslator -> Shape -> [Shape]
round3 st = map (transform st . (st !)) . round2 st

transform :: Map Shape a -> Shape -> Shape
transform st shape = findTrans [shape, a, b, c, d, e, f, g, h]
  where
    twoOps s = (reverse . transpose $ s, transpose s)
    (a, b) = twoOps shape
    (c, d) = twoOps a
    (e, f) = twoOps c
    (g, h) = twoOps e
    findTrans [] = error "transformation not found"
    findTrans (x:xs)
      | x `member` st = x
      | otherwise = findTrans xs

parseRule :: Parser ([String], [String])
parseRule = do
  rule <- split <$> parsePart `sepBy` string " => "
  optional eol
  return rule

split :: [[String]] -> ([String], [String])
split [a, b] = (a, b)

parsePart :: Parser [String]
parsePart = many (char '.' <|> char '#') `sepBy` char '/'

count :: Shape -> Int
count = length . concatMap (filter (== '#'))

makeTripleTranslator :: SimpleTranslator -> TripleTranslator
makeTripleTranslator st =
  mapWithKey (\k _ -> round3 st k) . filterWithKey (\k _ -> length k == 3) $ st

iterate5 :: SimpleTranslator -> Int
iterate5 st =
  sum . concatMap (map count . round2 st) . round3 st . transform st
    $ initialState

threeRounds :: TripleTranslator -> [(Int, Shape)] -> [(Int, Shape)]
threeRounds tt groups = result
  where
    result =
      map (foldr sumShapes (0, []))
        . groupBy (\a b -> snd a == snd b)
        . sortBy (comparing snd)
        . concatMap (\(a, b) -> map (a, ) . (tt !) $ b)
        $ groups
    sumShapes (v, s) (av, _) = (av + v, s)

manyRounds :: TripleTranslator -> Shape -> [[(Int, Shape)]]
manyRounds tt shape = iterate (threeRounds tt) [(1, shape)]

iterate18 :: SimpleTranslator -> Int
iterate18 st =
  sum . map (\(a, b) -> a * count b) . (!! 6) . manyRounds tt $ shape
  where
    tt = makeTripleTranslator st
    shape = transform st initialState

part1 :: Bool -> String -> String
part1 _ = show . iterate5 . fromList . parseByLine parseRule

part2 :: Bool -> String -> String
part2 _ = show . iterate18 . fromList . parseByLine parseRule
