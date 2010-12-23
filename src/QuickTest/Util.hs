module QuickTest.Util where

import Control.Monad (filterM)

partitionM :: (Functor m, Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs = do
  ps    <- filterM  p xs
  notPs <- filterM (fmap not . p) xs
  return (ps, notPs)

