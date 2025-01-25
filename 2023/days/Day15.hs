{-# LANGUAGE TemplateHaskell #-}

module Day15
  ( part1
  , part2
  ) where

import           Control.Applicative       (some, (<|>))
import           Data.Bits                 ((.&.))
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (pack)
import           Data.Char                 (ord)
import           Data.IntMap               (IntMap, empty, foldrWithKey, insert,
                                            notMember, (!))
import           Data.List                 (foldl')
import           Data.Sequence             (Seq ((:<|), (:|>)), breakl,
                                            foldrWithIndex, singleton, tails,
                                            (><))
import qualified Data.Sequence             as Sq (filter, null)
import           FlatParse.Basic           (anyAsciiDecimalInt, isLatinLetter,
                                            optional_, satisfy, switch)
import qualified FlatParse.Basic           as FB (anyAsciiChar, char, runParser)
import           FlatParse.Stateful        (get, modify, put)
import qualified FlatParse.Stateful        as FS (anyAsciiChar, char, runParser)
import           Helpers.Parsers.FlatParse (Parser, ParserS, extract, extractS)

type Label = ByteString

type Hash = Int

type Focal = Int

type Boxes = IntMap Box

type Box = Seq (Label, Focal)

parseValue :: ParserS () Int
parseValue =
  ($(FS.char ',') >> get >>= \v -> put 0 >> (v +) <$> parseValue)
    <|> ($(FS.char '\n') >> get)
    <|> (FS.anyAsciiChar >>= \v ->
           modify (\x -> ((ord v + x) * 17) .&. 255) >> parseValue)

parseBoxes :: Boxes -> Parser Int
parseBoxes boxes =
  (parseLabel >>= \l ->
     parseOp l boxes >>= \b -> optional_ $(FB.char ',') >> parseBoxes b)
    <|> ($(FB.char '\n') >> (pure . scoreBoxes $ boxes))

parseOp :: (Label, Hash) -> Boxes -> Parser Boxes
parseOp label boxes =
  $(switch
      [|case _ of
          "-" -> pure $ removeFromBox label boxes
          "=" ->
            anyAsciiDecimalInt >>= \focal ->
              pure . insertInBox label focal $ boxes|])

parseLabel :: Parser (Label, Hash)
parseLabel =
  some (satisfy isLatinLetter) >>= \l ->
    pure (pack l, foldl' (\acc c -> ((ord c + acc) * 17) .&. 255) 0 l)

insertInBox :: (Label, Hash) -> Focal -> Boxes -> Boxes
insertInBox (label, hash) focal boxes
  | hash `notMember` boxes = insert hash (singleton (label, focal)) boxes
  | Sq.null . Sq.filter ((== label) . fst) $ box =
    insert hash (box :|> (label, focal)) boxes
  | otherwise = insert hash ((before :|> (label, focal)) >< after) boxes
  where
    box = boxes ! hash
    (before, _ :<| after) = breakl ((== label) . fst) box

removeFromBox :: (Label, Hash) -> Boxes -> Boxes
removeFromBox (label, hash) boxes
  | hash `notMember` boxes = boxes
  | Sq.null . Sq.filter ((== label) . fst) $ box = boxes
  | otherwise = insert hash (before >< after) boxes
  where
    box = boxes ! hash
    (before, _ :<| after) = breakl ((== label) . fst) box

scoreBox :: Box -> Int
scoreBox = foldrWithIndex (\index (_, v) acc -> (index + 1) * v + acc) 0 -- foldr ((+) . sum) 0 . tails . fmap snd

scoreBoxes :: Boxes -> Int
scoreBoxes = foldrWithKey (\k v acc -> (k + 1) * scoreBox v + acc) 0

part1 :: Bool -> ByteString -> String
part1 _ = show . extractS . FS.runParser parseValue () 0

part2 :: Bool -> ByteString -> String
part2 _ = show . extract . FB.runParser (parseBoxes empty)
