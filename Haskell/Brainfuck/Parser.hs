module Brainfuck.Parser (Cursor, Instruction(..), decrementValue, incrementValue, getValue, new, nextPointer, parse, previousPointer, setValue, while) where

import Data.Char (chr, ord)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

type Error = String
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

errorMessage :: Char -> Index -> String
errorMessage c i = intercalate " " ["Instruction", [c], "at index", show i, "is invalid"]

parse :: Source -> Either Error [Instruction]
parse content = sequence instructions
  where (_, instructions) = doCompile content (0, [])

doCompile :: Source -> (Index, [Either Error Instruction]) -> (Index, [Either Error Instruction])
doCompile content (i, instructions) =
  if i < length content
     then
     case content !! i of
       '>' -> doCompile content ((i + 1), (Right NextPointer):instructions)
       '<' -> doCompile content ((i + 1), (Right PreviousPointer):instructions)
       '+' -> doCompile content ((i + 1), (Right IncrementValue):instructions)
       '-' -> doCompile content ((i + 1), (Right DecrementValue):instructions)
       '.' -> doCompile content ((i + 1), (Right GetValue):instructions)
       ',' -> doCompile content ((i + 1), (Right SetValue):instructions)
       '[' ->
          let (i', instructions') = doCompile content ((i + 1), [])
            in case sequence instructions' of
                 Right instructions'' -> doCompile content (i', (Right (While instructions'')):instructions)
                 Left error -> ((i + 1), ((Left error):instructions))
       ']' -> ((i + 1), reverse instructions)
       c   -> (i, ((Left (errorMessage c i)):instructions))
     else (i, reverse instructions)

