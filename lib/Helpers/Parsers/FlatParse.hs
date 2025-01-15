module Helpers.Parsers.FlatParse
  ( Parser
  , extract
  ) where

import           Data.Void
import           FlatParse.Basic (Result (OK))
import qualified FlatParse.Basic as F (Parser)

type Parser = F.Parser Void

extract :: Result Void a -> a
extract (OK result _) = result
extract _             = error "parser failed"
