module StateQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import Control.Monad.State

newtype Queue a = Queue ([a], [a])

empty :: Queue a
empty = Queue ([], [])

isEmpty :: State (Queue a) Bool
isEmpty = do
    Queue (xs, ys) <- get
    put (Queue (xs, ys))
    return (null xs && null ys)

enqueue :: a -> State (Queue a) ()
enqueue x = do
    Queue (xs,ys) <- get
    put (Queue (xs, x:ys))

dequeue :: State (Queue a) a
dequeue = do
    Queue q <- get
    let (e, qs) = extract q
    put qs
    return e
        where
            extract ([], ys) = let (x:xs) = reverse ys in (x, Queue (xs, []))
            extract (x:xs, ys) = (x, Queue (xs, ys))

        
