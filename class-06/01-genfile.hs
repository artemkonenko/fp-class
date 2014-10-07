{-
  В параметрах командной строки заданы целое число N, текстовая строка и имя файла.
  Создать файл с заданным именем, содержащий N-кратное повторение заданной строки.

  Совет: для запуска функции main в ghci с заданием параметров командной строки удобно
  использовать команду интерпретатора :main, например:

     ghci> :main 1000 "Привет, мир" hello.txt
-}
import System.Environment

createFile :: Int -> String -> FilePath -> IO ()
createFile n s fname = undefined

main = do
  [n_str, text, fname] <- getArgs
  createFile (read n_str) text fname
