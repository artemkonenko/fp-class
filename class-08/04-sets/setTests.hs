import System.Environment
import System.Random
import AbstractSet
import qualified ListSet as LS

randList :: Int -> IO [Int]
randList n = do
        g <- newStdGen
        return $ take n (randomRs (-n, n) g)

setTest1 :: ( AbstractSet s ) => s Int -> Bool
setTest1 s = member 5 $ foldr insert s [1..10]

main = do
    rlist <- getArgs >>= randList . read . head 

    putStr "set test #1: "
    print $ setTest1 (empty :: LS.Set Int)

    return ()