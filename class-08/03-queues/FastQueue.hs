module FastQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import AbstractQueue

newtype Queue a = Queue ([a], [a])

instance AbstractQueue Queue where
  empty = Queue ([], [])
  isEmpty (Queue (xs, ys)) = null xs && null ys
  enqueue (Queue (xs, ys)) x = Queue (xs, x:ys)
  dequeue (Queue ([], ys)) = let (x:xs) = reverse ys in (x, Queue (xs, []))
  dequeue (Queue (x:xs, ys)) = (x, Queue (xs, ys))
