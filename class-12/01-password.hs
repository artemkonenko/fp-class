{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Char

isValid :: String -> Bool
isValid s = length s >= 8 && 
                any isAlpha s && 
                any isNumber s && 
                any isPunctuation s

getValidPassword :: MaybeT IO String
getValidPassword = do
  lift $ putStrLn "Введите новый пароль:"
  s <- lift getLine
  guard (isValid s)
  return s
 
askPassword :: MaybeT IO ()
askPassword = do
  value <- msum $ repeat getValidPassword
  lift $ putStrLn "Сохранение в базе данных..."

main = runMaybeT askPassword
