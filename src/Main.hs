#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad
import System.Environment (getArgs)
import System.Process (readProcess)
import System.Directory (doesFileExist)

import Data.List
import Data.Function (on)

import Text.Printf

import QuickTest.Util
import QuickTest.Monad
import QuickTest.Types

flags :: [String]
flags = ["-names", "+verbose"]

main :: IO ()
main = do
  (flags', others) <- partition (`elem` flags) `fmap` getArgs
  (files, ghcOpts) <- partitionM doesFileExist others

  let opts = optionsFromFlags flags'
  -- Turn down GHC verbosity, because it messes up quicktest output.
  let ghcOpts' =  "-v0" : ghcOpts
  mapM_ (quickTestFile opts ghcOpts') files

quickTestFile :: Options -> GHCOptions -> FilePath -> IO ()
quickTestFile opts ghcOpts file = do
  names <- getNames `fmap` readFile file
  let snippets = runQuickTest (QuickTestState file names opts) quickTest
  unless (null snippets) $ do
    ghci ghcOpts file snippets >>= putStr

quickTest :: QuickTest ()
quickTest = do
  file <- asks qtsSourceFile
  props <- getProps
  emit (":load " ++ file)
  mapM_ execProp props

execProp :: Prop -> QuickTest ()
execProp prop = do
  displayProp prop
  verbose <- askOption optVerbose
  let impl = if verbose then "verboseCheck" else "quickCheck"
  emit $ printf "Test.QuickCheck.%s %s" impl (propName prop)

displayProp :: Prop -> QuickTest ()
displayProp prop = do
  showNames <- askOption optShowNames
  when showNames $ do
    emit $ printf "putStr \"%s \"" (show prop)

getNames :: Snippet -> [(String, LineNumber)]
getNames = nubBy ((==) `on` fst)
         . filter (not . null . fst)
         . flip zip [1..]
         . map (fst . head . lex)
         . lines

getProps :: QuickTest [Prop]
getProps = do
  file <- asks qtsSourceFile
  names <- asks qtsSourceNames
  return [Prop name file line | (name, line) <- names, "prop_" `isPrefixOf` name]

ghci :: GHCOptions -> FilePath -> [Snippet] -> IO String
ghci opts file snippets = do
  let snippets' = printf ":load %s" file : snippets
  readProcess "ghci" opts (unlines snippets')

optionsFromFlags :: [String] -> Options
optionsFromFlags args =
  Options {
      optShowNames = "-names" `notElem` args
    , optVerbose   = "+verbose" `elem` args
  }
