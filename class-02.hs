-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms sec = ( h, m, s )
			where 
				h = div sec 3600
				m = flip div 60 $ mod sec 3600
				s = mod sec 60

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

length :: Point -> Point -> Double
length (x1, y1) (x2, y2) = let
							a = flip (^) 2 $ (-) x1 x2
							b = flip (^) 2 $ (-) y1 y2
						   in sqrt $ (+) a b
						
-- triangle :: Point -> Point -> Point -> (Double, Double)
triangle a b c = (p, s)
  where
	la = Main.length a b
	lb = Main.length a c
	lc = Main.length b c
	p  = la + lb + lc
	halfp = p / 2
	s = sqrt $ halfp * (halfp - la) * (halfp - lb) * (halfp - lc)

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = (+) cur $ nEven xs 
		where
			cur
				| even x = 1
				| otherwise = 0

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = x * 2 : doubleElems xs


-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = cur ++ fltOdd xs
	where
		cur
			| odd x = [x]
			| otherwise = []


-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
fltRmNeg :: Integral a => [a] -> [a]
fltRmNeg [] = []
fltRmNeg (x:xs) = cur ++ fltRmNeg xs
	where
		cur
			| x >= 0 = [x]
			| otherwise = []

-- б) увеличить элементы с чётными значениями в два раза;
rltTwiceEven :: Integral a => [a] -> [a]
rltTwiceEven [] = []
rltTwiceEven (x:xs) = cur ++ rltTwiceEven xs
	where
		cur
			| x >= 0 = [x]
			| otherwise = []

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
fltSwapPairs :: Integral a => [a] -> [a]
fltSwapPairs [] = []
fltSwapPairs (x1:x2:xs) = [x2] ++ [x1] ++ fltSwapPairs xs
fltSwapPairs (x1) = []

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x + y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_pair :: [Integer] -> [Integer] -> [(Integer, Integer)]
combine_pair [] ys = []
combine_pair xs [] = []
combine_pair (x:xs) (y:ys) = (x, y) : combine_pair xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
getReverseFirstN :: Integer -> [Integer]
getReverseFirstN 0 = []
getReverseFirstN n = n : getReverseFirstN (n - 1)

-- б) в порядке возрастания.
getFirstN :: Integer -> [Integer]
getFirstN n = reverse $ getReverseFirstN n

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
spamAinList :: a -> [a] -> [a]
spamAinList y (x:xs) = x : y : spamAinList y xs
spamAinList y x = x

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
-- б) Eq a => [a] -> a -> Bool
-- в) [a] -> Int -> [a]
-- г) a -> Int -> [a]
-- д) [a] -> [a] -> [a]
-- е) Eq a => [a] -> [[a]]
-- ж) [a] -> [(Int, a)]
-- з) Eq a => [a] -> [a]