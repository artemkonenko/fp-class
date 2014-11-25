{-
   Тип Parser может быть определён следуюшим образом:
-}

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = undefined
  p >>= q = undefined
  fail _ = undefined

instance MonadPlus Parser where
  mzero = undefined
  p `mplus` q = undefined
