{-# LANGUAGE NamedFieldPuns #-}
module QuickTest.Types where

import Text.Printf

type Snippet = String
type GHCOptions = [String]
type LineNumber = Int

data QuickTestState
  = QuickTestState {
      qtsSourceFile :: FilePath
    , qtsSourceNames :: [(String, LineNumber)]
    , qtsOptions :: Options
  }

data Prop
  = Prop { propName :: String, propFile :: FilePath, propLine :: LineNumber }

instance Show Prop where
  show Prop { propName, propFile, propLine } = printf "%s:%s:%d" propFile propName propLine

data Options
  = Options {
      optShowNames :: Bool
    , optVerbose :: Bool
  }

