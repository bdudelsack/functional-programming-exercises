module Stream where
  data Stream a = Cons a (Stream a)

  streamToList :: Stream a -> [a]
  streamToList (Cons x xs) = x : streamToList xs

  repeatStream :: a -> Stream a
  repeatStream x = Cons x (repeatStream x)

  mapStream :: (a -> b) -> Stream a -> Stream b
  mapStream f (Cons x xs) = Cons (f x) (mapStream f xs)

  iterateStream :: (a -> a) -> a -> Stream a
  iterateStream f x = Cons x (iterateStream f (f x))

  nats :: Stream Integer
  nats = iterateStream (+1) 0

  interleaveStream :: Stream a -> Stream a -> Stream a
  interleaveStream (Cons x xs) ys = Cons x (interleaveStream ys xs)

  ruler :: Stream Integer
  ruler = interleaveStream (repeatStream 0) (mapStream (+1) ruler)
