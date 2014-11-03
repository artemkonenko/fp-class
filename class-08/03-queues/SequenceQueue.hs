module SequenceQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import AbstractQueue
import qualified Data.Sequence as S

newtype Queue a = QueueImpl (S.Seq a)

instance AbstractQueue Queue where
  empty = QueueImpl S.empty
  isEmpty (QueueImpl s) = S.null s
  enqueue (QueueImpl s) x = QueueImpl (s S.|> x)
  dequeue (QueueImpl s) = let (x S.:< s') = S.viewl s in (x, QueueImpl s')
