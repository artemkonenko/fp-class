import System.Environment
import System.Random
import Control.Monad
import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SequenceQueue as SQ

checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> Bool
checkQueue q = lastElem (enqueue q 5) == 5
 where
  lastElem q = let (x, q') = dequeue q in
               if isEmpty q' then x else lastElem q'

randList :: Int -> IO [Int]
randList n = do
        g <- newStdGen
        return $ take n (randomRs (-n, n) g)

reverseQueue :: (AbstractQueue q, Num i) => q i -> q i
reverseQueue q
    | isEmpty q = q
    | otherwise = enqueue (reverseQueue . snd . dequeue $ q) (fst . dequeue $ q)


-- Список из очередей 
fillQueues :: (AbstractQueue q, Num i) => [i] -> q i -> [q i]
fillQueues rands q = map reverseQueue $ reverse $ foldl fillQ [] rands
    where
        fillQ [] rint = [enqueue empty rint]
        fillQ (x:xs) rint = enqueue x rint : x : xs

dischargeQueues :: (AbstractQueue q, Num i, Monad m) => [q i] -> [m (q i)]
dischargeQueues qs = map deqs (zip qs [0..])
    where
        deqs :: (AbstractQueue q, Num i, Monad m) => (q i, Int) -> m (q i)
        deqs (q, n) = foldr (<=<) return (replicate n (return . snd . dequeue) ) q

checkQueueEq :: (AbstractQueue q, Num i, Eq i) => [q i] -> Bool
checkQueueEq (q:qs) = fst $ foldl checkeq (True, q) qs
    where
        checkeq :: (AbstractQueue q, Num i, Eq i) => (Bool, q i) -> q i -> (Bool, q i)
        checkeq (r, a) b = (r && (isEmpty sa) && (isEmpty sb) && (fa == fb), b)
            where 
                (fa, sa) = dequeue a
                (fb, sb) = dequeue b

testQueue :: (AbstractQueue q, Num a, Eq a, Monad m) => [a] -> q a -> m Bool
testQueue rlist q = return ( fillQueues rlist q) >>= (sequence . dischargeQueues) >>= return . checkQueueEq


main = do
        rlist <- getArgs >>= randList . read . head 

        putStr "check all Queues: "
        print $ checkQueue (enqueue empty 10 :: Q.Queue Int) &&
                checkQueue (enqueue empty 10 :: FQ.Queue Int) &&
                checkQueue (enqueue empty 10 :: SQ.Queue Int)
        
        putStr "Q.Queue Int test: " >> testQueue rlist (empty :: Q.Queue Int) >>= print
        putStr "FQ.Queue Int test: " >> testQueue rlist (empty :: FQ.Queue Int) >>= print
        putStr "SQ.Queue Int test: " >> testQueue rlist (empty :: SQ.Queue Int) >>= print


        return ()
