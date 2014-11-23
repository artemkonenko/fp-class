module Taylor (taylorSeries, sin', cos') where

import Control.Monad.Writer

{-
    eps - Необходимая точность,
    startN - степень при x у первого слагаемого
    startS - первое слагаемое
    toNext m x n - множитель для перехода к следующему слагаемому
-}
taylorSeries :: Double -> Double -> Integer -> Double -> (Double -> Integer -> Double -> (Integer, Double)) -> Double -> Writer [Double] Double
taylorSeries eps x startN startS toNext res = tell [startS] >> if (abs ( (snd nextSN) - startS ) < eps) then
                                                                    return res
                                                               else
                                                                    taylorSeries eps x (fst nextSN) (snd nextSN) toNext (res + startS)
                                        where
                                            nextSN = toNext x startN startS


sin' :: Double -> (Double, [Double])
sin' x = runWriter $ taylorSeries 1e-12 x 1 x nxt 0
    where
        nxt :: Double -> Integer -> Double -> (Integer, Double)
        nxt y curN curS = ( curN+2, (-1) * curS * y * y / (fromInteger curN+1) / (fromInteger curN+2))

cos' :: Double -> (Double, [Double])
cos' x = runWriter $ taylorSeries 1e-12 x 0 1 nxt 0
    where
        nxt :: Double -> Integer -> Double -> (Integer, Double)
        nxt y curN curS = ( curN+2, (-1) * curS * y * y / (fromInteger curN+1) / (fromInteger curN+2))
