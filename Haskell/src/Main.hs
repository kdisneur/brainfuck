module Main where

import Brainfuck (evaluate, transpile, Language(..))
import Data.List (intercalate)
import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))

data Action
  = Evaluate
  | Transpile Language

main :: IO ()
main = do
  action <- fmap chooseAction getArgs
  content <- fmap cleanContent getContents
  executeAction action content
  return ()

chooseAction :: [String] -> Maybe Action
chooseAction ("evaluate" : others) = Just Evaluate
chooseAction ("-l" : "C" : "transpile" : others) = Just (Transpile C)
chooseAction others = Nothing

cleanContent :: String -> String
cleanContent = filter ((/=) '\n')

executeAction :: Maybe Action -> String -> IO ()
executeAction (Just Evaluate) content = either logError id $ evaluate content
executeAction (Just (Transpile language)) content = either logError putStrLn (transpile language content)
executeAction Nothing content = logError "Command doesn't exist.\n\
                                         \\n\
                                         \Usage: brainfuck evaluate <<< file.bf # Execute\n\
                                         \       brainfuck -l C transpile <<< file.bf # Transpile to C\n\
                                         \\n\
                                         \Available languages: C"

logError :: String -> IO ()
logError message = do
  hPutStrLn stderr message
  exitWith (ExitFailure 1)
  return ()
