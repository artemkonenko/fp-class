import Control.Monad.State
import StateQueue


queueTest :: State (Queue Integer) Integer
queueTest = do
    enqueue 1
    enqueue 2
    enqueue 3
    a <- dequeue
    b <- dequeue
    return (a + b)

main :: IO ()
main = do
    let a = runState queueTest ( empty :: Queue Integer)
    let b = runState queueTest (snd a)
    let c = runState queueTest (snd b)
    let d = runState queueTest (snd c)

    print $ [fst a, fst b, fst c, fst d] == [2,1,3,2]

    return ()
    