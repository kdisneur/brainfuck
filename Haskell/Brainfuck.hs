module Brainfuck (transpile, evaluate, Language(..)) where

import qualified Brainfuck.Parser as Parser
import qualified Brainfuck.Interpreter as Interpreter
import qualified Brainfuck.Transpiler.C as Transpiler.C

type Error = String
data Language
  = C

evaluate :: String -> Either Error (IO ())
evaluate = Interpreter.evaluate

transpile :: Language -> String -> Either Error String
transpile C = (fmap Transpiler.C.transpile) . Parser.parse
