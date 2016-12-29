module Functor (Functor, fmap) where
  import Prelude hiding(Functor (..))

  data Tree a = Empty | Node (Tree a) a (Tree a)

  instance Functor Maybe where
    fmap Nothing = Nothing
    fmap f (Just x) = Just (f x)

  instance Functor Tree where
    fmap Empty = Empty
    fmap f x v y = Node (fmap x) f v (fmap y)

  instance Functor (Either e) where
    fmap f (Right x) = Right (f x)
    fmap _ (Left x) = Left x
