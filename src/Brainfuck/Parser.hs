module Brainfuck.Parser (Cursor, Instruction(..), decrementValue, incrementValue, getValue, new, nextPointer, parse, previousPointer, setValue, while) where

import Data.Char (chr, ord)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

type Pointer = Int
type Index = Int
type Source = String
data Cursor = Cursor Pointer (Map.Map Pointer Int) deriving Show
data Instruction
  = DecrementValue
  | GetValue
  | IncrementValue
  | NextPointer
  | PreviousPointer
  | SetValue
  | While [Instruction]
  deriving Show

onPointer :: (Pointer -> Pointer) -> Cursor -> Cursor
onPointer f (Cursor x ys) = Cursor (f x) ys

onValue :: (Int -> Int) -> Cursor -> Cursor
onValue f original@(Cursor x ys) = Cursor x (Map.insert x y' ys)
  where y' = (f . get) original

get :: Cursor -> Int
get (Cursor x ys) = (fromMaybe 0 . Map.lookup x) ys

getValue :: Cursor -> Char
getValue = chr . get

setValue :: Char -> Cursor -> Cursor
setValue v = onValue (\x -> ord v)

decrementValue :: Cursor -> Cursor
decrementValue = onValue (flip (-) 1)

incrementValue :: Cursor -> Cursor
incrementValue = onValue ((+) 1)

nextPointer :: Cursor -> Cursor
nextPointer = onPointer ((+) 1)

previousPointer :: Cursor -> Cursor
previousPointer = onPointer (flip (-) 1)

new :: Cursor
new = Cursor 0 Map.empty

while :: (Cursor -> IO Cursor) -> Cursor -> IO Cursor
while f cursor = if 0 == (get cursor) then return cursor else (f cursor) >>= while f

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

