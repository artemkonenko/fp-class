module AbstractSet where

class AbstractSet s where
    empty  :: s a
    member :: Ord a => a -> s a -> Bool
    insert :: Ord a => a -> s a -> s a 
--  delete :: Ord a => a -> s a -> s a