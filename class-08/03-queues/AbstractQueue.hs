module AbstractQueue where

class AbstractQueue a where
  empty :: a t
  isEmpty :: a t -> Bool
  enqueue :: a t -> t -> a t
  dequeue :: a t -> (t, a t)
