module Day21
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Bifunctor       (second)
import           Data.Either          (fromRight)
import           Data.List            as L (filter, map)
import           Data.Set             as St (Set, difference, empty, filter,
                                             fromList, intersection, map,
                                             member, notMember, null, size,
                                             toList, union, unions)
import           Helpers.Parsers      (Parser)
import           Text.Megaparsec      (eof, many, manyTill, optional, parse,
                                       sepBy, try, (<|>))
import           Text.Megaparsec.Char (char, eol, letterChar, string)

import           Debug.Trace

type Ingredient = String

type Allergen = String

type Food = (Set Ingredient, Set Allergen)

parseList :: Parser [Food]
parseList = many parseFood <* eof

parseFood :: Parser Food
parseFood = do
  ingredients <- parseIngredients
  string "contains "
  allergens <- parseAllergens
  return (ingredients, allergens)

parseIngredients :: Parser (Set Ingredient)
parseIngredients =
  fromList <$> manyTill (manyTill letterChar (char ' ')) (char '(')

parseAllergens :: Parser (Set Allergen)
parseAllergens = fromList <$> manyTill parseAllergen eol

parseAllergen :: Parser Allergen
parseAllergen = do
  allergen <- many letterChar
  try (void . string $ ", " :: Parser ()) <|> (void . string $ ")" :: Parser ())
  return allergen

refinedSets :: [Food] -> Set (Allergen, Set Ingredient)
refinedSets foods =
  St.map
    (\a ->
       ( a
       , foldl1 intersection . L.map fst . L.filter (\(_, b) -> a `member` b) $
         foods)) .
  unions . L.map snd $
  foods

noAllergen :: [Food] -> Int
noAllergen foods =
  sum .
  L.map (\i -> length . L.filter (\f -> i `member` fst f) $ foods) .
  toList . difference allFoods $
  unsure
  where
    refined = refinedSets foods
    unsure = unions . St.map snd $ refined
    allFoods = unions . L.map fst $ foods

pairAllergens :: [Food] -> [Ingredient]
pairAllergens foods = L.map snd . toList . simplify $ (empty, refined)
  where
    refined = refinedSets foods
    simplify (paired, unpaired)
      | St.null unpaired = paired
      | otherwise = simplify (paired `union` newPaired, remaining)
      where
        newPaired =
          St.map (second (head . toList)) . St.filter ((== 1) . size . snd) $
          unpaired
        pruned = St.filter (\x -> fst x `notElem` St.map fst newPaired) unpaired
        remaining =
          St.map (second (St.filter (`notElem` St.map snd newPaired))) pruned

part1 :: Bool -> String -> String
part1 _ = show . noAllergen . fromRight [] . parse parseList ""

part2 :: Bool -> String -> String
part2 _ = show . pairAllergens . fromRight [] . parse parseList ""
