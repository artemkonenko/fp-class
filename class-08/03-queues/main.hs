import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ

checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> Bool
checkQueue q = lastElem (enqueue q 5) == 5
 where
  lastElem q = let (x, q') = dequeue q in
               if isEmpty q' then x else lastElem q'

main = print $
         checkQueue (enqueue empty 10 :: Q.Queue Int)
         &&  checkQueue (enqueue empty 10 :: FQ.Queue Int)
