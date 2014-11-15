{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment

readNumFile :: Num a => FilePath -> IO [a]
readNumFile = undefined

solve :: Num a => [[a]] -> (Int, [a])
solve = undefined

main = getArgs >>= mapM readNumFile >>= print.solve
