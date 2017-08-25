module Brainfuck.Zipper (currentCursor, empty, farLeft, farRight, insert, left, right, update, Zipper) where

import Data.Maybe (fromMaybe)

data Zipper a = Zipper [a] [a] deriving Show

currentCursor :: Zipper a -> Maybe a
currentCursor (Zipper _ (c:_)) = Just c
currentCursor _ = Nothing

empty :: Zipper a
empty = Zipper [] []

farLeft :: Zipper a -> Bool
farLeft (Zipper [] _) = True
farLeft _ = False

farRight :: Zipper a -> Bool
farRight (Zipper _ []) = True
farRight _ = False

insert :: a -> Zipper a -> Zipper a
insert x (Zipper l r) = Zipper l (x:r)

left :: Zipper a -> Zipper a
left (Zipper (x:l) r) = Zipper l (x:r)
left z = z

update :: a -> Zipper a -> Zipper a
update x (Zipper l (_:r)) = Zipper l (x:r)
update x (Zipper l []) = Zipper l [x]

right :: Zipper a -> Zipper a
right (Zipper l (x:r)) = Zipper (x:l) r
right z = z
