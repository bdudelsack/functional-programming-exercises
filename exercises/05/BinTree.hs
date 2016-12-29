module BinTree where
    data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

    test :: BinTree Int
    test = Node 6 (Node 3 Empty Empty) (Node 10 Empty Empty)

    -- berechnet Summe aller Werte in einem mit Zahlen beschrifteten Baum
    sumTree :: BinTree Int -> Int
    sumTree Empty = 0
    sumTree (Node v x y) = v + sumTree x + sumTree y

    -- liefert alle Werte eines Baumes in einer Liste zurück
    values :: BinTree a -> [a]
    values Empty = []
    values (Node v x y) = v : values x ++ values y

    -- wendet eine gegebene Funktion auf alle Werte im Baum an
    mapTree :: (a -> b) -> BinTree a -> BinTree b
    mapTree _ Empty = Empty
    mapTree f (Node v x y) = Node (f v) (mapTree f x) (mapTree f y)
