module Day16
  ( part1
  , part2
  ) where

import           Data.Char     (digitToInt, intToDigit)
import           Data.Sequence as Sq (Seq ((:<|), (:|>)), drop, dropWhileL,
                                      empty, fromList, length, null, singleton,
                                      splitAt, take, (><))

import           Debug.Trace

data Packet =
  Packet
    { version  :: Int
    , packetID :: Int
    , value    :: Value
    }
  deriving (Show)

data Value
  = Literal Int
  | Operator (Seq Packet)
  deriving (Show)

isLiteral :: Value -> Bool
isLiteral (Literal _) = True
isLiteral _           = False

fromLiteral :: Value -> Int
fromLiteral (Literal k) = k

fromOperator :: Value -> Seq Packet
fromOperator (Operator seq) = seq

decode :: Seq Char -> (Packet, Seq Char)
decode s@(a :<| b :<| c :<| d :<| e :<| f :<| p@(lengthID :<| sp))
  | [d, e, f] == "100" =
    ( Packet (fromBin (a :<| b :<| singleton c)) 4 (Literal . fromBin $ packLit)
    , restLit)
  | lengthID == '0' =
    ( Packet
        (fromBin (a :<| b :<| singleton c))
        (fromBin (d :<| e :<| singleton f))
        (Operator packByLength)
    , restLength)
  | lengthID == '1' =
    ( Packet
        (fromBin (a :<| b :<| singleton c))
        (fromBin (d :<| e :<| singleton f))
        (Operator packByNumber)
    , restNumber)
  where
    (packLit, restLit) = parseLit p
    packLength = fromBin . Sq.take 15 $ sp
    restLength = Sq.drop (15 + packLength) sp
    packByLength = parseByLength . Sq.take packLength . Sq.drop 15 $ sp
    (packNumber, toParseNumber) = Sq.splitAt 11 sp
    (packByNumber, restNumber) =
      parseByNumber toParseNumber (fromBin packNumber)

parseByLength :: Seq Char -> Seq Packet
parseByLength s
  | Sq.null s = empty
  | Sq.null rest = singleton parsed
  | otherwise = parsed :<| parseByLength rest
  where
    (parsed, rest) = decode s

parseByNumber :: Seq Char -> Int -> (Seq Packet, Seq Char)
parseByNumber rest 0 = (empty, rest)
parseByNumber s n = (packet :<| parsed, rest)
  where
    (packet, unparsed) = decode s
    (parsed, rest) = parseByNumber unparsed (n - 1)

parseLit :: Seq Char -> (Seq Char, Seq Char)
parseLit (a :<| as)
  | a == '1' = (Sq.take 4 as >< parsed, rest)
  | a == '0' = (Sq.take 4 as, Sq.drop 4 as)
  where
    (parsed, rest) = parseLit (Sq.drop 4 as)

fromBin :: Seq Char -> Int
fromBin = foldl (\a b -> digitToInt b + 2 * a) 0

toBin :: Int -> Seq Char
toBin 0 = singleton '0'
toBin 1 = singleton '1'
toBin x = toBin (div x 2) :|> intToDigit (mod x 2)

pad :: Seq Char -> Seq Char
pad s
  | Sq.length s == 4 = s
  | otherwise = pad ('0' :<| s)

sumVersions :: Packet -> Int
sumVersions p
  | isLiteral . value $ p = version p
  | otherwise =
    (+ version p) . sum . fmap sumVersions . fromOperator . value $ p

valPacket :: Packet -> Int
valPacket p
  | i == 0 = sum recVal
  | i == 1 = product recVal
  | i == 2 = minimum recVal
  | i == 3 = maximum recVal
  | i == 4 = fromLiteral . value $ p
  | i == 5 && fp > sp = 1
  | i == 5 = 0
  | i == 6 && fp < sp = 1
  | i == 6 = 0
  | i == 7 && fp == sp = 1
  | i == 7 = 0
  where
    i = packetID p
    recVal = fmap valPacket . fromOperator . value $ p
    (fp :<| sp :<| empty) = recVal

packets :: String -> Packet
packets =
  fst . decode . foldl (><) empty . map (pad . toBin . digitToInt) . init

part1 :: Bool -> String -> String
part1 _ = show . sumVersions . packets

part2 :: Bool -> String -> String
part2 _ = show . valPacket . packets
