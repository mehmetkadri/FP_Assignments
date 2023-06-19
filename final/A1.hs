{-# LANGUAGE GADTs #-}

module A1 where

import Prelude

data BTree a where
    Empty :: BTree a
    Node  :: a -> BTree a -> BTree a -> BTree a

printBTree :: Show a => BTree a -> String
printBTree t =
    case t of
        Empty      -> "_"
        Node n l r -> "[" ++ show n ++ "," ++ printBTree l ++ "," ++ printBTree r ++ "]" 

instance Show a => Show (BTree a) where
    show t = printBTree t

insertTree :: Ord a => BTree a -> a -> BTree a
insertTree t x =
    case t of
        Empty      -> Node x Empty Empty
        Node n l r -> if x > n 
                      then Node n l (insertTree r x) 
                      else if x == n 
                      then t
                      else Node n (insertTree l x) r

searchTree :: Ord a => [a] -> BTree a
searchTree l = foldl (insertTree) (Empty) l

unionTree :: Ord a => BTree a -> BTree a -> BTree a
unionTree t1 t2 =
    case t1 of
        Empty      -> t2
        Node n l r -> insertTree (unionTree (unionTree l r) t2) n

splitMaxFromTree :: Ord a => BTree a -> Maybe (a, BTree a)
splitMaxFromTree t =
    case t of
        Empty          -> Nothing
        Node n l Empty -> Just (n, l)
        Node n l r     -> let Just (m, r') = splitMaxFromTree r
                          in Just (m, Node n l r')

removeFromTree :: Ord a => a -> BTree a -> BTree a
removeFromTree x t =
    case t of
        Empty      -> Empty
        Node n l r ->
            case (compare x n) of
                EQ ->
                    case (splitMaxFromTree l) of
                        Nothing      -> r
                        Just (m, l') -> Node m l' r
                LT -> Node n (removeFromTree x l) r
                GT -> Node n l (removeFromTree x r)

differenceTree :: Ord a => BTree a -> BTree a -> BTree a
differenceTree t1 t2 =
    case t2 of
        Empty      -> t1
        Node n l r -> removeFromTree n (differenceTree (differenceTree t1 l) r)
