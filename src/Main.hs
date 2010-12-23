#!/usr/bin/env runhaskell
-- This file defines a command
--      quickCheck <options> <files>
-- which invokes quickCheck on all properties defined in the files given as
-- arguments, by generating an input script for hugs and then invoking it.
-- quickCheck recognises the options
--      +names     print the name of each property before checking it
--      +verbose   displays each test case before running
-- Other options (beginning with + or -) are passed unchanged to ghci.
--

import Text.Printf
import System.Environment (getArgs)
import System.Process (readProcess)
import Data.List (nub, partition, isPrefixOf)

main :: IO ()
main = do
  (options, files) <- partition isOption `fmap` getArgs
  mapM_ (process options) files

process :: [String] -> FilePath -> IO ()
process opts file = do
  source <- readFile file

  let (showNames, opts') = getOption "+names" opts
      (verbose, opts'')  = getOption "+verbose" opts'
      showProp name      = if showNames then printf "putStr \"%s: \"" name else "return ()"
      quickCheckImpl     = if verbose then "verboseCheck" else "quickCheck"
      quickCheck name    = printf "%s >> Test.QuickCheck.%s %s" (showProp name) quickCheckImpl name

      names = nub
            . filter isProp
            . map firstWord
            . map unliterate
            . lines
            $ source

  if null names
    then do
      return ()
    else do
      let input = unlines $ printf ":load %s" file : map quickCheck names
      readProcess "ghci" opts'' input >>= putStr

-- ugly hack for .lhs files, is there a better way?
unliterate :: String -> String
unliterate ('>' : line) = line
unliterate line         = line

isOption (c : _) = c `elem` "+-"
isOption _       = False

firstWord :: String -> String
firstWord = fst . head . lex

isProp :: String -> Bool
isProp = isPrefixOf "prop_" . unliterate

getOption :: String -> [String] -> (Bool, [String])
getOption option options = (option `elem` options, filter (/= option) options)
