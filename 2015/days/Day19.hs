{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Day19
  ( part1
  , part2
  ) where

import           Control.Monad             (unless, void)
import           Data.Bifunctor            (bimap)
import           Data.Bits                 (countLeadingZeros, finiteBitSize,
                                            popCount)
import           Data.ByteString           (ByteString)
import           Data.ByteString.UTF8      (fromString)
import           Data.Char                 (isAlpha, isLowerCase)
import           Data.List                 (partition)
import           Data.Map                  (Map, insertWith, member, (!))
import qualified Data.Map                  as M (insert)
import           Data.Maybe                (fromJust, isNothing)
import           Data.Sequence             (Seq ((:<|), (:|>)), fromList, (><))
import qualified Data.Sequence             as Sq (null)
import           Data.Set                  (Set)
import qualified Data.Set                  as S (insert, size)
import           FlatParse.Basic           (char, failed, isLatinLetter, many,
                                            optional, optional_, runParser,
                                            satisfy, skip, some, string, switch,
                                            takeLine, (<|>))
import           Helpers.Parsers.FlatParse (extract, extractS)
import qualified Helpers.Parsers.FlatParse as F (Parser, ParserS)

import           Debug.Trace

type ParserMol = F.ParserS Formulas Int

type Parser = F.Parser (Formulas, Molecule)

type Parser' = F.Parser [WordAtom]

type Formulas = Map Atom [Molecule]

type Formulas' = Map Molecule Atom

type Molecule = Seq Atom

type Atom = String

type Back = Map (Int, Atom, Atom)

-- Atoms that do not transform once they are reached are C (which has to be at
-- the beginning of a word), Rn, Ar and Y. If Rn is present, then Ar is always
-- present, and if Y is present, then it is always between Rn and Ar. Rn, Y and
-- Ar are always separated by exactly one atom. From this,
-- we can build a parser.
-- Once we have our successful atomparsing, we can count the steps of reduction
-- without actually bothering about the actual path : it takes one step to
-- reduce two continuous to one or a C(RnAr) molecule to one or a 1 + RnAr
-- molecule to one, so we actually only care about the length of the
-- continuous streams of molecules and are not going to need the dictionary at
-- all for part 2, we can directly parse our molecule sequence.
data WordAtom
  = RnAr [[WordAtom]]
  | Continuous Int
  deriving (Show)

type AtomParser = F.Parser WordAtom

isContinuous :: WordAtom -> Bool
isContinuous (Continuous _) = True
isContinuous _              = False

parseInput :: Parser
parseInput =
  parseFormulas >>= \x ->
    many parseAtom <* $(char '\n') >>= \y -> pure (x, fromList y)

parseInput' :: Parser'
parseInput' = parseFormulas' >> some atomParser <* $(char '\n')

parseFormulas :: F.Parser Formulas
parseFormulas = ($(char '\n') >> pure mempty) <|> parseLine

parseFormulas' :: F.Parser ()
parseFormulas' = void $(char '\n') <|> (void takeLine >> parseFormulas')

parseLine :: F.Parser Formulas
parseLine = do
  from <- parseAtom
  $(string " => ")
  to <- fromList <$> some parseAtom
  $(char '\n')
  insertWith (++) from [to] <$> parseFormulas

parseAtom :: F.Parser Atom
parseAtom =
  satisfy isAlpha >>= \x ->
    optional (satisfy isLowerCase) >>= \y ->
      if isNothing y
        then pure [x]
        else pure [x, fromJust y]

multifold :: Formulas -> Molecule -> Molecule -> Set Molecule
multifold formulas seen toSee
  | Sq.null toSee = mempty
  | m `member` formulas =
    foldr (S.insert . molecule) (multifold formulas seen' toSee') (formulas ! m)
  | otherwise = multifold formulas seen' toSee'
  where
    (m :<| toSee') = toSee
    seen' = seen :|> m
    molecule k = seen >< k >< toSee'

atomParser :: AtomParser
atomParser =
  $(switch
      [|case _ of
          "Rn" -> RnAr <$> rnArParser
          "\n" -> failed
          _    -> Continuous . length <$> some parseNonSepAtom|])

parseNonSepAtom :: F.Parser Atom
parseNonSepAtom = do
  atom <- parseAtom
  if atom `elem` ["Rn", "Y", "Ar"]
    then failed
    else pure atom

rnArParser :: F.Parser [[WordAtom]]
rnArParser =
  some (some atomParser >>= \x -> optional_ parseY >> pure x) >>= \x ->
    parseAr >> pure x

parseY :: F.Parser ()
parseY = parseAtom >>= \a -> unless (a == "Y") failed

parseAr :: F.Parser ()
parseAr = parseAtom >>= \a -> unless (a == "Ar") failed

reduceAll :: [WordAtom] -> Int
reduceAll molecules =
  length (filter isContinuous molecules) + reduce molecules - 1

reduce :: [WordAtom] -> Int
reduce =
  uncurry (+)
    . bimap reduceContinuous (sum . map reduceRnAr)
    . partition isContinuous

-- it takes 0 steps to reduce a 1 atom molecule to 1 atom. Let's assume it takes (n-1)
-- steps to reduce an n atoms molecule to 1 atom. Then it takes (n + n - 1) = 2n
-- -1 steps to reduce a 2n atoms molecule to 1 atom, and (1 + reduce (2m)) =  (1 + 2n - 1) = 2n steps to
-- reduce a (2n + 1) atoms molecule to 1 atom. QED
reduceRnAr :: WordAtom -> Int
reduceRnAr (RnAr molecules) = (+ 1) . sum . map reduce $ molecules

reduceContinuous :: [WordAtom] -> Int
reduceContinuous = (+ negate 1) . sum . map fromContinuous
  where
    fromContinuous (Continuous atoms) = atoms

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . S.size
    . (\(a, b) -> multifold a mempty b)
    . extract
    . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ = show . reduce . extract . runParser parseInput'
