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
  (action, file) <- fmap chooseAction getArgs
  content <- readFile file
  executeAction action content
  putStrLn ""

chooseAction :: [String] -> (Maybe Action, String)
chooseAction ("evaluate" : file : others) = (Just Evaluate, file)
chooseAction ("-l" : "c" : "transpile" : file : others) = (Just (Transpile C), file)
chooseAction ("-l" : "js" : "transpile" : file : others) = (Just (Transpile JS), file)
chooseAction others = (Nothing, "")

cleanContent :: String -> String
cleanContent = filter ((/=) '\n')

executeAction :: Maybe Action -> String -> IO ()
executeAction (Just Evaluate) content = evaluate content
executeAction (Just (Transpile language)) content = putStrLn (transpile language content)
executeAction Nothing content = logError "Command doesn't exist.\n\
                                         \\n\
                                         \Usage: brainfuck evaluate file.bf # Execute\n\
                                         \       brainfuck -l c transpile file.bf # Transpile to C\n\
                                         \\n\
                                         \Available languages: c, js"

logError :: String -> IO ()
logError message = do
  hPutStrLn stderr message
  exitWith (ExitFailure 1)
  return ()
