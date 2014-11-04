module ListSet (Set, empty, member, insert) where

import AbstractSet

newtype Set t = SetImpl [t]

instance AbstractSet Set where
  empty = SetImpl []
  member x (SetImpl xs) = elem x xs
  insert x (SetImpl xs) = if member x (SetImpl xs) then (SetImpl xs) else  SetImpl (x : xs)