module Helpers.Parsers.FlatParse
  ( Parser
  , ParserS
  , debug
  , extract
  , extractS
  ) where

import           Data.Void
import           FlatParse.Basic    (Result (OK), traceLine)
import qualified FlatParse.Basic    as F (Parser)
import qualified FlatParse.Stateful as FS (Parser, Result (OK))
import Debug.Trace

type Parser = F.Parser Void

type ParserS a b = FS.Parser a Void b

extractS :: FS.Result b a -> a
extractS (FS.OK result _ _) = result

extract :: Result b a -> a
extract (OK result _) = result
extract _             = error "parser failed"

debug :: String -> F.Parser a () 
debug a = do
  t <- traceLine
  trace (a ++ t) pure ()
