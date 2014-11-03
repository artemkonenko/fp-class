import System.Environment
import System.Random
import Control.Monad
import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ

main = do
        getArgs >>= return.head >>= putStrLn >> return ()