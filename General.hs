module General
  ( preciseTimeIt
  ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           System.TimeIt          (timeItT)
import           Text.Printf            (printf)

preciseTimeIt :: (MonadIO m, Show a) => Int -> m a -> m a
preciseTimeIt prec ioa = do
  (t, a) <- timeItT ioa
  liftIO $ printf ("CPU time" ++ ":%6." ++ show prec ++ "fs\n") t
  return a
