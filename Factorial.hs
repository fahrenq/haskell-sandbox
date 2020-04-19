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

{-

Hey,
So I dived deeper to the “Weak Head Normal Form” stuff in Haskell and just for the sake of ensuring I understood it myself I’ll write down what is it here.
So WHNF is an expression outermost part (action) of which is either data constructor or a lambda. I’ve been struggling a little on how to identify “outermost” part, but as I understood its something that happens last if we execute program kinda linearly. So examples of WHNF will be:

  100                              -- outermost part is an Integer constructor
  (100 + 100, 100 - 100)           -- outermost part is tuple constructor
  CustomType { something = True }  -- outermost part is a CustomType constructor
  (\x -> x)                        -- outermost part is lambda

Operator $! evaluates expression on the right side to WHNF, for example: (100 + 100, 100 - 100) will be evaluated to (100 + 100, 100 - 100), because it’s already in WHNF, but expression 100 + 100 is not WHNF as outermost part is (+), so it will be evaluated to Integer constructor 200.
Let’s now look at our examples.

Factorial:
  fac' :: (Integral a) => a -> a -> a
  fac' 0 acc = acc
  fac' n acc = fac' (n - 1) (n * acc)
  fac'' :: (Integral a) => a -> a -> a
  fac'' 0 acc = acc
  fac'' n acc = fac'' (n - 1) $! n * acc

  {-
  fac' 5 1
    fac' (5 - 1) (5 * 1)
      fac' (4 - 1) (4 * (5 * 1))
        fac' (3 - 1) (3 * (4 * (5 * 1)))
          fac' (2 - 1) (2 * (3 * (4 * (5 * 1))))
            fac' (1 - 1) (1 * (2 * (3 * (4 * (5 * 1)))))
              (1 * (2 * (3 * (4 * (5 * 1)))))
  $! makes it do
  fac' 5 1
    fac' (5 - 1) 5
      fac' (4 - 1) 20
        fac' (3 - 1) 60
          fac' (2 - 1) 120
            fac' (1 - 1) 120
              120
  -}

If we expand (n * acc) we’ll see this pattern: (1 * (2 * (3 * (4 * ...)). Closest WHNF in this expression is integer constructor, so it actually does the multiplication on every step.

Infinite list:
  xs = 1 : xs
  take 10 $! xs
1 : xs evaluated to WHNF will still be 1 : xs since : is just a list constructor. Which means we’re actually doing nothing in this case with $! function. Important note here: (10 + 10) : xs evaluated to WHNF will still be (10 + 10) : xs, because expression is already is in WHNF and $! only considers expression as a whole but not parts of it, which is a common pitfall when doing fold using tuple as an accumulator, even if we strict-apply it, it always stays a tuple with big chains of operations as its elements.

Thanks for sending me to this rabbit hole. Pretty interesting things.
-}

i = 10000000

main = defaultMain
  [ bgroup
      ("fac of " ++ show i)
      [ -- bench "fac basic" $ whnf fac i
        bench "fac with acc" $ whnf (\() -> fac' i 1) ()
      , bench "fac with acc $!" $ whnf (\() -> fac'' i 1) ()
      ]
  ]
