module Factorial where

fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

fac' :: (Integral a) => a -> a -> a
fac' 0 acc = acc
fac' n acc = fac' (n - 1) (n * acc)

fac'' :: (Integral a) => a -> a -> a
fac'' 0 acc = acc
fac'' n acc = fac'' (n - 1) $! n * acc

main = putStrLn "a"
