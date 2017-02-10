-- ====================
-- SICP, Chapter 1.2
-- ====================


-- ========================================
-- Recursion and Iteration
-- ========================================

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factIter :: Integer -> Integer -> Integer -> Integer
factIter prod count max
  | count > max = prod
  | otherwise = factIter (count * prod) (count + 1) max

tailRecFact :: Integer -> Integer
tailRecFact = factIter 1 1

factIter' :: Integer -> Integer -> Integer
factIter' acc n
  | n == 0     = acc
  | otherwise = factIter' (acc*n) (n-1)

tailRecFact' :: Integer -> Integer
tailRecFact' = factIter' 1

fact' :: Integer -> Integer
fact' n = foldr (*) 1 [1..n]

-- ========================================
-- Tree Recursion
-- ========================================

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-1)

fib' :: Integer -> Integer
fib' n = fibList !! (fromIntegral n)
  where
    fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

