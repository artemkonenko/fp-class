module AbstractGraph (AbstractGraph, mkGraph, edgeIn, adjacent, nodes, weight, nodesBounds, edges, fromGraph) where

import Data.Ix

-- АТД «Граф» (Graph n w, где n — тип вершин, w — тип весов) имеет следующий интерфейс:
class AbstractGraph g where

{-
 * Первый параметр определяет, является ли граф направленным (если False, то дуги должны
   дублироваться на противоположное направление);
 * второй параметр задаёт начальное и конечное значение для множества вершин;
 * третий параметр — это список дуг с весами. -}
  mkGraph :: (Ix n, Num w) => Bool -> (n, n) -> [(n, n, w)] -> g n w

-- возвращается True, если в графе имеется дуга между заданными вершинами;
  edgeIn :: (Ix n, Num w) => g n w -> (n, n) -> Bool

-- возвращается список смежных с данной вершин;
  adjacent :: (Ix n, Num w) => g n w -> n -> [n]

-- возвращается список вершин графа;
  nodes :: (Eq w, Ix n, Num w) => g n w -> [n]

-- возвращается вес дуги между заданными вершинами
  weight :: (Ix n, Num w) => n -> n -> g n w -> w

-- возвращается список дуг графа (с учётом ориентированности);
  edges :: (Ix n, Num w) => g n w -> [(n,n,w)]

-- преобразователь из графа с другим представлением
  fromGraph :: (Ix n, Num w, AbstractGraph g') => g' n w -> g n w

-- начальное и конечное значения для множества вершин
  nodesBounds :: (Ix n, Num w) => g n w -> (n, n)
