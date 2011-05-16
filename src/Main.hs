{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

import Control.Monad
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import System.Directory (doesFileExist)
import System.Exit

import Data.List
import Data.Function (on)
import Data.Maybe (isJust)

import Text.Printf

import QuickTest.Util
import QuickTest.Monad
import QuickTest.Types
import Data.Either

flags :: [String]
flags = ["-names", "+verbose"]

main :: IO ()
main = do
  (flags', others) <- partition (`elem` flags) `fmap` getArgs
  (files, ghcOpts) <- partitionM doesFileExist others

  let opts = optionsFromFlags flags'
  -- Turn down GHC verbosity, because it messes up quicktest output.
  let ghcOpts' =  "-v0" : ghcOpts
  success <- (null . lefts) `fmap` mapM (quickTestFile opts ghcOpts') files
  if success then exitSuccess
    else exitFailure

quickTestFile :: Options -> GHCOptions -> FilePath -> IO (Either String ())
quickTestFile opts ghcOpts file = do
  names <- getNames `fmap` readFile file
  let snippets = runQuickTest (QuickTestState file names opts) quickTest
  if null snippets then return $ Right ()
    else do
    (exitCode, stdout, stderr) <- ghci ghcOpts snippets
    printResult stdout stderr
    return $ handleResult exitCode stdout stderr

printResult::String -> String -> IO ()
printResult out err = putStr out >> putStr err

handleResult::ExitCode -> String -> String -> Either String ()
handleResult ExitSuccess stdout _stderr = parseResult stdout
handleResult _err _stdout stderr = Left stderr
           
parseResult::String -> Either String ()
parseResult output | isSuccess = Right () 
                   | otherwise = Left output
                     where
                       isSuccess = "+++ OK, passed" `isInfixOf` output

quickTest :: QuickTest ()
quickTest = do
  file <- asks qtsSourceFile
  load file
  getProps >>= mapM_ execProp

execProp :: Prop -> QuickTest ()
execProp prop = do
  displayProp prop
  verbose <- askOption optVerbose
  (if verbose then verboseCheck else quickCheck) prop

displayProp :: Prop -> QuickTest ()
displayProp prop = do
  showNames <- askOption optShowNames
  when showNames $ do
    emit $ printf "putStr \"%s \"" (show prop)

getProps :: QuickTest [Prop]
getProps = do
  file <- asks qtsSourceFile
  names <- asks qtsSourceNames
  return [Prop name file line | (name, line) <- names, "prop_" `isPrefixOf` name]

ghci :: GHCOptions -> [Snippet] -> IO (ExitCode, String, String)
ghci opts snippets = do
  readProcessWithExitCode "ghci" opts (unlines snippets)

getNames :: Snippet -> [(String, LineNumber)]
getNames = nubBy ((==) `on` fst)
         . map (\(Just name, line) -> (name, line))
         . filter (isJust . fst)
         . flip zip [1..]
         . map getName
         . lines

getName :: Snippet -> Maybe String
getName (lex -> (name, _) : _) = Just name
getName _                      = Nothing

optionsFromFlags :: [String] -> Options
optionsFromFlags args =
  Options {
      optShowNames = "-names" `notElem` args
    , optVerbose   = "+verbose" `elem` args
  }
