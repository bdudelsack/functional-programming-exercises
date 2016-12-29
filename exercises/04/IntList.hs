module IntList where
  type IntSet = Int -> Bool

  -- liefert eine leere Menge
  empty :: IntSet
  empty _ = False

  -- testet, ob ein Element in der Menge enthalten ist
  isElem :: IntSet -> Int -> Bool
  isElem set = set

  -- liefert eine Menge mit einem Element
  singleton :: Int -> IntSet
  singleton i x = x == i

  -- fuegt ein Element zu einer Menge hinzu
  insert :: Int -> IntSet -> IntSet
  insert i set x = (x == i) || set x

  -- entfernt ein Element aus einer Menge
  remove :: Int -> IntSet -> IntSet
  remove i set x = (x /= i) && set x

  -- vereinigt zwei Mengen
  union :: IntSet -> IntSet -> IntSet
  union s1 s2 x = s1 x || s2 x

  -- berechnet den Schnitt von zwei Mengen
  intersect :: IntSet -> IntSet -> IntSet
  intersect s1 s2 x = s1 x && s2 x

  -- liefert zu einer Liste von ganzen Zahlen die Menge, die die gleichen Elemente enthaelt
  listToSet :: [Int] -> IntSet
  listToSet = foldr (union . singleton) empty

  -- Koennen Sie insert mit Hilfe der anderen Funktionen definieren?
  insert' :: Int -> IntSet -> IntSet
  insert' i =  union (singleton i)
