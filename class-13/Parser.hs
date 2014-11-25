module Parser where

import Control.Applicative hiding (many, optional)
import Control.Monad

newtype Parser a = Parser { apply :: String -> [(a, String)] }

parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  p >>= q = Parser (\s ->
               [ (y, s'') | (x, s') <- apply p s,
                            (y, s'') <- apply (q x) s'])
  fail _ = Parser (\s -> [])

instance MonadPlus Parser where
  mzero = Parser (\s -> [])
  p `mplus` q = Parser (\s -> let ps = apply p s in if null ps then apply q s else ps)

instance Functor Parser where
    fmap = liftM
 
instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero
