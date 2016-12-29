module RNA where

  data Base = Uracil | Adenin | Cytosin | Guanin deriving (Show)

  type RNA = [Base]

  parseBase :: Char -> Maybe Base
  parseBase 'U' = Just Uracil
  parseBase 'A' = Just Adenin
  parseBase 'C' = Just Cytosin
  parseBase 'G' = Just Guanin
  parseBase _ = Nothing

  parseRNA :: String -> Maybe RNA
  parseRNA = foldr (reducer . parseBase) (Just [])

  reducer :: Maybe Base -> Maybe RNA -> Maybe RNA
  reducer Nothing _ = Nothing
  reducer _ Nothing = Nothing
  reducer (Just x) (Just xs) = Just (x : xs)

  foldMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
  foldMaybe _ Nothing = Nothing
  foldMaybe f (Just a) = f a

  (>>-) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>-) x f = foldMaybe f x
