module Helpers.Parsers.FlatParse
  ( Parser
  , ParserS
  , debug
  , debugS
  , extract
  , extractS
  ) where

import           Data.Void
import           Debug.Trace
import qualified FlatParse.Basic    as F (Parser, Result (OK), traceLine)
import qualified FlatParse.Stateful as FS (Parser, Result (OK), traceLine)

type Parser = F.Parser Void

type ParserS a b = FS.Parser a Void b

extractS :: FS.Result b a -> a
extractS (FS.OK result _ _) = result
extractS _                  = error "parser failed"

extract :: F.Result b a -> a
extract (F.OK result _) = result
extract _               = error "parser failed"

debug :: String -> F.Parser a ()
debug a = do
  t <- F.traceLine
  trace (a ++ t) pure ()

debugS :: String -> FS.Parser a b ()
debugS a = do
  t <- FS.traceLine
  trace (a ++ t) pure ()
