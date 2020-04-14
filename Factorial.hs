module Main where

import           Criterion.Main


fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

fac' :: (Integral a) => a -> a -> a
fac' 0 acc = acc
fac' n acc = fac' (n - 1) (n * acc)

fac'' :: (Integral a) => a -> a -> a
fac'' 0 acc = acc
fac'' n acc = fac'' (n - 1) $! n * acc

i = 10000000

main = defaultMain
  [ bgroup
      ("fac of " ++ show i)
      [ -- bench "fac basic" $ whnf fac i
        bench "fac with acc" $ whnf (\() -> fac' i 1) ()
      , bench "fac with acc $!" $ whnf (\() -> fac'' i 1) ()
      ]
  ]
