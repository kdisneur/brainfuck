module Brainfuck.Transpiler.C (transpile) where

import Brainfuck.Parser (Instruction(..))
import Data.List (intercalate)

doTranspile :: Instruction -> [String] -> [String]
doTranspile DecrementValue = insert "--*ptr;"
doTranspile GetValue = insert "putchar(*ptr);"
doTranspile IncrementValue = insert "++*ptr;"
doTranspile NextPointer = insert "++ptr;"
doTranspile PreviousPointer = insert "--ptr;"
doTranspile SetValue = insert "*ptr=getchar();"
doTranspile (While instructions) = insert ("while(*ptr){" ++ generate instructions ++ "}")

generate :: [Instruction] -> String
generate = intercalate "\n" . foldr (doTranspile) []

header :: [String]
header = [ "#include<stdio.h>"
         , "int main(){"
         , "char array[30000] = {0};"
         , "char *ptr=array;"]

footer :: [String]
footer = [ "putchar('\\n');"
         , "return 0;"
         , "}"]

insert :: String -> [String] -> [String]
insert = (:)

transpile :: [Instruction] -> String
transpile instructions = intercalate "\n" (header ++ [generate instructions] ++ footer)
