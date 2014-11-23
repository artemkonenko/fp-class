module STRef_sorts (shakerSort) where

import Control.Monad.ST

import Data.Array
import Data.Array.ST

swapElems :: Ix i => i -> i -> STArray s i e -> ST s ()
swapElems i j arr = do
    vi <- readArray arr i
    vj <- readArray arr j

    writeArray arr i vj
    writeArray arr j vi    

makeArr :: (MArray a e m) => [e] -> m (a Int e)
makeArr list = newListArray (0, length list - 1) list


shakerSort :: (Ord a) => [a] -> [a]
shakerSort list = undefined {- elems $ runSTArray $ do
    arr <- makeArr list
    let n = length list

    forM_ (indices arr) \i -> do
        forM_ (reverse indices arr) \j -> do -}



shakerSortTest = shakerSort [4,3,1,5,2] == [1,2,3,4,5]