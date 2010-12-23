{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
module QuickTest.Monad
  ( module QuickTest.Types
  , QuickTest
  , runQuickTest
  , asks
  , emit
  , askOption
  ) where

import Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer

import QuickTest.Types

newtype QuickTest a = QuickTest { runQT :: WriterT [Snippet] (Reader QuickTestState) a }
  deriving (Monad, MonadWriter [Snippet], MonadReader QuickTestState)

runQuickTest :: QuickTestState -> QuickTest () -> [Snippet]
runQuickTest st = snd . flip runReader st . runWriterT . runQT

emit :: Snippet -> QuickTest ()
emit = tell . lines

askOption :: (Options -> a) -> QuickTest a
askOption f = asks (f . qtsOptions)

