module Brainfuck.Interpreter (evaluate, run) where

import Brainfuck.Parser (Cursor, Instruction(..), decrementValue, incrementValue, getValue, new, nextPointer, parse, previousPointer, setValue, while)
import Control.Monad (foldM, (>=>))

run :: [Instruction] -> IO Cursor
run instructions = do
  cursor <- runWith instructions new
  return cursor

runWith :: [Instruction] -> Cursor -> IO Cursor
runWith instructions cursor = do
  foldM (flip doRunWith) cursor instructions

doRunWith :: Instruction -> Cursor -> IO Cursor
doRunWith NextPointer cursor = return . nextPointer $ cursor
doRunWith PreviousPointer cursor = return . previousPointer $ cursor
doRunWith IncrementValue cursor = return . incrementValue $ cursor
doRunWith DecrementValue cursor = return . decrementValue $ cursor
doRunWith GetValue cursor = do
  let value = getValue cursor
  putStr [value]
  return cursor
doRunWith SetValue cursor = do
  value <- getChar
  let cursor' = setValue value cursor
  return cursor'
doRunWith (While instructions) cursor = while f cursor
  where fs = map doRunWith instructions
        f  = foldl (>=>) return fs

evaluate :: String -> IO ()
evaluate content = (doEvaluate . parse) content

doEvaluate :: [Instruction] -> IO ()
doEvaluate instructions =
  run instructions >> return ()
