module Brainfuck.Transpiler.JS (transpile) where

import Brainfuck.Parser (Instruction(..))
import Data.List (intercalate)

doTranspile :: Instruction -> [String] -> [String]
doTranspile DecrementValue = insert "dec(c);"
doTranspile GetValue = insert "putchar(c);"
doTranspile IncrementValue = insert "inc(c);"
doTranspile NextPointer = insert "next(c);"
doTranspile PreviousPointer = insert "prev(c);"
doTranspile SetValue = insert "await getchar(c);"
doTranspile (While instructions) = insert ("while(get(c) != 0){" ++ generate instructions ++ "}")

generate :: [Instruction] -> String
generate = intercalate "" . foldr (doTranspile) []

header :: [String]
header = [ "#! /usr/bin/env node\n"
         , "let c={p: 0, m: {}};"
         , "const stdin = process.stdin;"
         , "stdin.setRawMode(true);"
         , "stdin.resume();"
         , "stdin.setEncoding('utf8');"
         , "const get = (c) => c.m[c.p] || 0;"
         , "const dec = (c) => c.m[c.p] = get(c) - 1;"
         , "const inc = (c) => c.m[c.p] = get(c) + 1;"
         , "const next = (c) => c.p++;"
         , "const prev = (c) => c.p--;"
         , "const getchar = (c) => new Promise(resolve => {\
                                   \ stdin.on('data', (k) => {\
                                     \ c.m[c.p] = k.charCodeAt(0);\
                                     \ resolve();\
                                   \})});"
         , "const putchar = (c) => process.stdout.write(String.fromCharCode(get(c)));"
         , "(async () => {"]

footer :: [String]
footer = [ "process.stdout.write('\\n');"
         , "process.exit(0);"
         , "})();"]

insert :: String -> [String] -> [String]
insert = (:)

transpile :: [Instruction] -> String
transpile instructions = intercalate "" (header ++ [generate instructions] ++ footer)
