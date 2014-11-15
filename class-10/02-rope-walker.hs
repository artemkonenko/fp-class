import Control.Monad 
import Control.Monad.Instances
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Either String Pole
updatePole p = if unbalanced p then reason p else Right p
  where
    unbalanced (l, r) = abs (l - r) > balance
    reason (l, r) = if l < r then Left "Not enouth the birds on left side" else Left "Not enouth the birds on rigth side"

landBoth :: Birds -> Birds -> Pole -> Either String Pole
landBoth l r (left, right) = updatePole (left + l, right + r)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n p = landBoth n 0 p

landRight :: Birds -> Pole -> Either String Pole
landRight n p = landBoth 0 n p

unlandAll :: Pole -> Either String Pole
unlandAll p = updatePole (0, 0)

banana :: Pole -> Either String Pole
banana = const (Left "Banana")

tests = all test [1..5]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Left "unbalance"
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "Banana"
    test 4 = (return (0, 0) >>= landBoth 5 8 >>= landLeft 1) == Right (6, 8)
    test 5 = (return (0, 0) >>= landBoth 14 14 >>= unlandAll >>= landRight 2 >>= landLeft 4) == Right (4, 2)

load :: String -> Maybe Pole
load fname = undefined
