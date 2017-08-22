module Brainfuck (transpile, evaluate, Language(..)) where

import qualified Brainfuck.Parser as Parser
import qualified Brainfuck.Interpreter as Interpreter
import qualified Brainfuck.Transpiler.C as Transpiler.C

data Language
  = C

evaluate :: String -> IO ()
evaluate = Interpreter.evaluate

transpile :: Language -> String -> String
transpile C = Transpiler.C.transpile . Parser.parse
