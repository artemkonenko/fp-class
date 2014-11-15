{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}
type Alphabet = [Char]
type State = Int
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

-- пример НКА
nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]

-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect = undefined

-- в дальнейшем может пригодиться функция whileM,
-- которая собирает результаты в список, пока истинно
-- заданное условие
whileM :: m Bool -> m a -> m [a]
whileM = undefined

-- Напишите функцию, определяющую, допускает ли НКА заданное слово 
accept :: NFA -> String -> Bool
accept = undefined

-- Постройте ещё как минимум три примера НКА
nfa1 :: NFA
nfa1 = undefined

nfa2 :: NFA
nfa2 = undefined

nfa3 :: NFA
nfa3 = undefined

{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify = undefined
