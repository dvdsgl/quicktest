module QuickTest.Util where

import Control.Monad (filterM)

partitionM p xs = do
  ps    <- filterM  p xs
  notPs <- filterM (fmap not . p) xs
  return (ps, notPs)

