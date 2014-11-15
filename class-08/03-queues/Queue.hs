module Queue (Queue, empty, enqueue, dequeue, isEmpty) where

import AbstractQueue

newtype Queue t = QueueImpl [t]

instance AbstractQueue Queue where
  empty = QueueImpl []

  isEmpty (QueueImpl xs) = null xs

  enqueue (QueueImpl xs) x = QueueImpl (xs ++ [x])

  dequeue (QueueImpl (x:xs)) = (x, QueueImpl xs)

