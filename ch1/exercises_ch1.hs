-- ========================================
-- Exercises, Chapter 1.1, SICP
-- Elements of Programming
-- ========================================

-- ========================================
-- Ex 1.2
-- ========================================

-- In Haskell operators can be enclosed in parents to make them
-- prefix; however, their arity is fixed (whereas in scheme the arity
-- of mathematical arithmetic like `+` and `/` is arbitrary). One can
-- `hack` an arbitrary arity version of any arithmetic operator using
-- a list (of arbitrary size), and a fold, if a function is not
-- already defined in the standard libraries.

ex12 =
  (/) (sum [5, 4, ((-) 2 ((-) 3 ((+) 6 ((/) 4 5))))]) (product [3, ((-) 6 2), ((-) 2 7)])

-- ========================================
-- Ex 1.3
-- ========================================

sumSquaresMax x y z
  | x >= y && y >= z = sumSquares x y
  | x >= y && z >= y = sumSquares x z
  | otherwise     = sumSquares y z
  where
    sumSquares x y = x^2 + y^2

-- ========================================
-- Ex 1.4
-- ========================================

aPlusAbsB a b =
  (if b > 0 then (+) else (-)) a b

-- ========================================
-- Ex 1.5
-- ========================================

p = p

test x y = if x == 0 then 0 else y

ex15 = test 0 p

-- In scheme, the evaluation order is applicative, hence the
-- equivalent function would evaluate both arguments to test before
-- substituting them into the body of test during beta-reduction. As
-- the function p is non-terminating, the call to `test 0 p` would
-- also be non-terminating.

-- In Haskell, the evaluation scheme is lazy (basically, normal,
-- modulo some differences which are not relevant here). This means
-- that the arguments to test are not evaluated until they are needed.
-- Since the first argument to test is equal to zero, the function
-- will execute the first branch of the if-else expression and return
-- 0 without ever evaluating the else branch.

-- ========================================
-- Ex 1.6
-- ========================================

mySqrt x = sqrtIter 1.0 x

sqrtIter guess x
  | goodEnough guess x = guess
  | otherwise = sqrtIter (improve guess x) x

goodEnough guess x = abs (guess^2 - x) < 0.001

improve guess x = average guess (x / guess)

average x y = (x + y) / 2

newIf predicate thenClause elseClause
  | predicate = thenClause
  | otherwise = elseClause

newSqrtIter guess x =
  newIf (goodEnough guess x) guess (newSqrtIter (improve guess x) x)

newSqrt x = newSqrtIter 1.0 x

-- In Scheme, newIf is evaluated as a function, which means that its
-- arguments are evaluated before they are substituted in for its
-- formal paramters when its body is evaluated. This means that any
-- recursive call that relies on newIf (such as in newSqrtIter)would
-- never terminate, since it would simply evaluate its arguments
-- forever.

-- In Haskell (which follows a lazy evaluation scheme, which is
-- similar to the normal evaluation scheme described in SICP),
-- arguments are only evaluated when they are needed, so newSqrtIter,
-- which calls newIf, is a function that always terminates for
-- well-formed inputs.

-- ========================================
-- Ex 1.7
-- ========================================

betterGoodEnough guess oldGuess =
  abs ((guess - oldGuess) / guess) < 0.0001

betterSqrtIter guess oldGuess x
  | betterGoodEnough guess oldGuess = guess
  | otherwise = betterSqrtIter (improve guess x) guess x

betterSqrt x = betterSqrtIter 1.0 0.5 x

-- ========================================
-- Ex 1.8
-- ========================================

cubeRoot = cubebRootIter 1.0 0.5

cubeRootIter guess oldGuess x
  | betterGoodEnough guess oldGuess = guess
  | otherwise = cubeRootIter (improveCube guess x) guess x

improveCube guess x =
  (2*guess + guess^2/x) / 3

-- ========================================
-- Lexically Scoped Rewrite of sqrt
-- ========================================

squareRoot x = squareIter 1.0
  where
    squareIter guess
      | goodEnough guess = guess
      | otherwise = squareIter (improve guess)
    goodEnough guess = abs (guess^2 - x) < 0.001
    improve guess = average guess (/ x guess)
    average x y = (x + y) / 2
