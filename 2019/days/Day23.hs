module Day23
  ( part1
  , part2
  ) where

import           Data.Bifunctor  (first)
import           Data.IntMap     as M (IntMap, findWithDefault, fromList,
                                       member, singleton)
import           Data.List       (groupBy, sortBy)
import           Data.List.Split (chunksOf)
import           Intcode         (Intcode, clearOutput, initialise, input,
                                  runIntcode, sendInput, sendMultInput)

type Network = [([Packet], Intcode)]

type Packet = (Address, [Int])

type Address = Int

type NatNetwork = (NatPacket, Network, Int)

type NatPacket = [Int]

isIdle :: Network -> Bool
isIdle = all (\x -> null (input . snd $ x) && null (fst x))

packetise :: [Int] -> [Packet]
packetise = map (\[a, b, c] -> (a, [b, c])) . chunksOf 3 . reverse

initialiseNetwork :: String -> Network
initialiseNetwork string =
  map (first packetise . runIntcode . flip sendInput nic) [0 .. 49]
  where
    nic = initialise string

initialiseNat :: String -> NatNetwork
initialiseNat string = ([], initialiseNetwork string, -1)

extractPackets :: [Packet] -> (NatPacket, IntMap [Int])
extractPackets pack = (natPacket, fromList packets)
  where
    packets =
      map (foldr (\(a, b) (_, c) -> (a, b ++ c)) (0, [])) .
      groupBy (\(a, _) (b, _) -> a == b) .
      sortBy (\(a, _) (b, _) -> compare a b) $
      pack
    natPackets = map snd . filter ((== 255) . fst) $ packets
    natPacket
      | null natPackets = []
      | otherwise = reverse . take 2 . reverse . head $ natPackets

receiveSend :: Network -> Int
receiveSend network
  | not (null natPacket) = natPacket !! 1
  | otherwise = receiveSend . recSend packets $ machines
  where
    (natPacket, packets) = extractPackets . concatMap fst $ network
    machines = map (clearOutput . snd) network

natReceiveSend :: NatNetwork -> Int
natReceiveSend (nat, network, seen)
  | isIdle network && not (null nat) && last nat == seen = last nat
  | isIdle network && not (null nat) = natReceiveSend ([], sendNat, last nat)
  | not (null natPacket) =
    natReceiveSend (natPacket, recSend packets machines, seen)
  | otherwise = natReceiveSend (nat, recSend packets machines, seen)
  where
    sendNat = recSend (singleton 0 nat) machines
    machines = map (clearOutput . snd) network
    (natPacket, packets) = extractPackets . concatMap fst $ network

recSend :: IntMap [Int] -> [Intcode] -> Network
recSend packets =
  zipWith
    (\a b ->
       first packetise .
       runIntcode . sendMultInput (findWithDefault [-1] a packets) $
       b)
    [0 .. 49]

part1 :: Bool -> String -> String
part1 _ = show . receiveSend . initialiseNetwork

part2 :: Bool -> String -> String
part2 _ = show . natReceiveSend . initialiseNat
