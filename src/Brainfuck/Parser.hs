module Brainfuck.Parser (Cursor, Instruction(..), decrementValue, incrementValue, getValue, new, nextPointer, parse, previousPointer, setValue, while) where

import Brainfuck.Zipper (currentCursor, empty, farLeft, farRight, insert, left, right, update, Zipper)
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type Pointer = Int
type Index = Int
type Source = String
type Cursor = Zipper Int
data Instruction
  = DecrementValue
  | GetValue
  | IncrementValue
  | NextPointer
  | PreviousPointer
  | SetValue
  | While [Instruction]
  deriving Show

defaultValue :: Int
defaultValue = 0

get :: Cursor -> Int
get = (fromMaybe defaultValue) . currentCursor

getValue :: Cursor -> Char
getValue = chr . get

setValue :: Char -> Cursor -> Cursor
setValue v = update (ord v)

decrementValue :: Cursor -> Cursor
decrementValue c = update x c
  where x = (get c) - 1

incrementValue :: Cursor -> Cursor
incrementValue c = update x c
  where x = (get c) + 1

nextPointer :: Cursor -> Cursor
nextPointer c = if farRight c
                   then insert defaultValue c
                   else right c

previousPointer :: Cursor -> Cursor
previousPointer c = if farLeft c
                       then insert defaultValue c
                       else left c

new :: Cursor
new = insert defaultValue empty

while :: (Cursor -> IO Cursor) -> Cursor -> IO Cursor
while f c = if 0 == (get c) then return c else (f c) >>= while f

parse :: Source -> [Instruction]
parse content = instructions
  where (_, instructions) = doCompile content (0, [])

doCompile :: Source -> (Index, [Instruction]) -> (Index, [Instruction])
doCompile content (i, instructions) =
  if i < length content
     then
     case content !! i of
       '>' -> doCompile content ((i + 1), NextPointer:instructions)
       '<' -> doCompile content ((i + 1), PreviousPointer:instructions)
       '+' -> doCompile content ((i + 1), IncrementValue:instructions)
       '-' -> doCompile content ((i + 1), DecrementValue:instructions)
       '.' -> doCompile content ((i + 1), GetValue:instructions)
       ',' -> doCompile content ((i + 1), SetValue:instructions)
       '[' ->
          let (i', instructions') = doCompile content ((i + 1), [])
           in doCompile content (i', (While instructions'):instructions)
       ']' -> ((i + 1), reverse instructions)
       c   -> doCompile content ((i + 1), instructions)
     else (i, reverse instructions)
