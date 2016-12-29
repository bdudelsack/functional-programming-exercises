{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

main :: IO ()
main = return ()

-- größter gemeinsamer Teiler

ggT :: Integer -> Integer -> Integer
ggT a b
 | b == 0      = a
 | otherwise   = ggT b (a `mod` b)
 
-- kleinstes gemeinsames Vielfaches

kgV :: Integer -> Integer -> Integer
kgV a b = (a * b) `div` (ggT a b)

-- Fibonacci Zahlen

fib :: Integer -> Integer
fib x
 | x == 0      = 0
 | x <= 2      = 1
 | otherwise   = fib (x-1) + fib (x-2)