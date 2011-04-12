{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
module QuickTest.Monad
  ( module QuickTest.Types
  , QuickTest
  , runQuickTest
  , asks
  , emit
  , askOption
  , load
  , quickCheck
  , verboseCheck
  ) where

import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer

import Text.Printf

import QuickTest.Types

newtype QuickTest a = QuickTest { runQT :: WriterT [Snippet] (Reader QuickTestState) a }
  deriving (Monad, MonadWriter [Snippet], MonadReader QuickTestState)

runQuickTest :: QuickTestState -> QuickTest () -> [Snippet]
runQuickTest st = snd . flip runReader st . runWriterT . runQT

askOption :: (Options -> a) -> QuickTest a
askOption f = asks (f . qtsOptions)

emit :: Snippet -> QuickTest ()
emit = tell . lines

load :: FilePath -> QuickTest ()
load file = emit (":load " ++ file)

quickCheck :: Prop -> QuickTest ()
quickCheck Prop { propName = name } = emit $ printf "Test.QuickCheck.quickCheck %s" name

verboseCheck :: Prop -> QuickTest ()
verboseCheck Prop { propName = name } = emit $ printf "Test.QuickCheck.verboseCheck %s" name

