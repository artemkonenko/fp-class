import System.Environment
import Control.Monad.Reader

-- Какой-то правильный способ я находил на первой контрольной, но сегодня hackage мне не помог.
split :: (Eq a) => a -> [a] -> [[a]]
split separator line = if end == [] then [e] else e : (split separator $ tail end )
    where
        e = takeWhile (separator/=) line
        end = dropWhile (separator/=) line

parseConfig :: [String] -> [(String, Int)]
parseConfig cfg = map (split '=') cfg >>= (\[name, value] -> [(name, read value)]) 

evalOp :: (String, Int) -> Int -> Int
evalOp ("summand", a) = (+a)
evalOp ("multiplier", a) = (*a)
evalOp ("divisor", a) = (`div` a)
                        

evaluator :: [Int] -> Reader [(String, Int)] [Int]
evaluator nums = do
    cfg <- ask

    return $ ( nums >>= (\x -> [foldl (flip evalOp) x cfg]) )

main = do
    [configFile, numbersFile] <- getArgs
    config <- (readFile configFile >>= return . parseConfig . lines)
    numbers <- (readFile numbersFile >>= return . (map (read :: String -> Int)) . lines )

    print $ runReader (evaluator numbers) config
    return ()