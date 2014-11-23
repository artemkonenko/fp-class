module TestStateQueue (tests) where

import Distribution.TestSuite
import Control.Monad.State
import StateQueue

queueOddTest :: State (Queue Integer) Integer
queueOddTest = do
    enqueue 1
    enqueue 2
    enqueue 3
    a <- dequeue
    b <- dequeue
    return $ a + b

tests :: IO [Test]
tests = return [Test oddTest]
    where
        oddTest = TestInstance
            { name = "OddTest"
            , run = return $ Finished $ do
                                            let a = runState queueOddTest ( empty :: Queue Integer)
                                            let b = runState queueOddTest (snd a)
                                            let c = runState queueOddTest (snd b)
                                            let d = runState queueOddTest (snd c)

                                            if ([fst a, fst b, fst c, fst d] == [3,4,5,3]) then Pass else Fail (show [fst a, fst b, fst c, fst d])
            , tags = []
            , options = []
            , setOption = \_ _ -> Right oddTest
            }