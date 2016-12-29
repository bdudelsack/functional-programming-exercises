{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

import           BinTree

-- Aufgabe 2

testTree :: BinTree Int
testTree = Node 10 (Node 5 Empty Empty) (Node 3 Empty Empty)

main :: IO ()
main = return ()

minTree :: BinTree Int -> Int
minTree Empty        = maxBound
minTree (Node x y z) = min x (min (minTree y) (minTree z))

replace :: BinTree a -> b -> BinTree b
replace Empty _        = Empty
replace (Node x y z) v = Node v (replace y v) (replace z v)

replaceMinRec :: BinTree Int -> a -> (BinTree a, Int)
replaceMinRec t v = (t', m)
  where
    m = minTree t
    t' = replace t v


-- wie benutze ich hier replaceMinRec?

replaceMin :: BinTree Int -> BinTree Int
replaceMin t = t'
  where
    m = minTree t
    t' = replace t m


-- Aufgabe 2

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldl (\xs x -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldl (\xs x -> if p x then x : xs else xs) []

reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

concat' :: [a] -> [a] -> [a]
concat' = foldr (\x xs -> xs ++ [x])

concat'' :: [a] -> [a] -> [a]
concat'' = foldl (flip (:))
