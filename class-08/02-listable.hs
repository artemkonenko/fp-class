{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
  toList  :: a -> [a]
  fromList:: [a] -> a

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}

instance Listable String where
  toList = words
  fromList = unwords

instance Listable Integer where
  toList a = if abs a < 10 then [abs a] else (toList (abs a `div` 10) ) ++ [abs a `mod` 10]
  fromList = foldl (\a b ->a*10 + b) 0
