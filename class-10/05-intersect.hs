{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
intersect :: Eq a => [[a]] -> [a]
intersect = foldr undefined undefined
