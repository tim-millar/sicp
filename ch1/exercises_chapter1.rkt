;; ========================================
;; Exercises, Chapter 1.1, SICP
;; Elements of Programming
;; ========================================

#lang sicp


;; Ex 1.1

10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; => 16

;; Ex 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Ex 1.3

(define (sum-squares-max x y z)
  (cond ((and (>= x z) (>= y z)) (sum-squares x y))
        ((and (>= x y) (>= z y)) (sum-squares x z))
        ((and (>= y x) (>= z x)) (sum-squares y z))))

(define (sum-squares x y)
  (+ (square x) (square y)))

(define (square x)
  (* x x))

;; Ex 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; if b is positive, sum a and b
;; if b is non-positive, subtract b from a

;; Ex 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))

;; If the evaluation order is applicative, the call to (test 0 (p))
;; will first evaluate its arguments before substituting the results
;; in the body of the test function. p is a function that calls itself
;; recursively and never terminates, so the call to test will never
;; terminate, because it will never finish evaluating its arguments.

;; If the evaluation order is normal, the call to (test 0 (p)) will first
;; substitute its arguments in the body of the test function, and evaluate
;; as far as it can until it needs to evaluate its arguments. Since x equals
;; zero, the predicate will evaluate to true and the consequent will be returned.
;; The alternative is the non-terminating procedure, which will never e evaluated.

;; ========================================

;; Ex. 1.6

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) epsilon))

(define epsilon 0.001)

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqr-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqr-iter (improve guess x) x)))

(define (new-sqrt x)
  (new-sqr-iter 1.0 x))

;; Any call to new-sqr will ultmately call new-if. Since new-if is a fuction,
;; it is evaluated according to the normal evaluation scheme. This means that
;; its arguments are first evaluated and then substituted for formal parameters
;; in the body of the function (beta-reduction). One of these arguments is a
;; recursive call. This argument must be fully evaluated before the function body
;; can be executed, entering an infinite loop, and so the function is
;; non-terminatng.

;; ========================================
;; Ex. 1.7
;; ========================================

;; Suppose the argument to `sqrt' was very large. Then the function
;; `good-enough' would be testing to see whether the absolute
;; difference between the value we want the square root of and the
;; square of the guess is less than some small value epsilon. Since
;; these two numbers are very large, we may well lack the precision to
;; tell if there is epsilon diference between them. An example would
;; be using unsigned 32-bit integers, which range from 0 to 2^32 - 1.
;; If the number we want to find the square root of is larger than
;; 2^32 (say), then the difference between that and the square of the
;; guess will be zero, even if the guess is still far off.

;; Suppose now that the argument to `sqrt' was very small. Then the
;; function `good-enough' would be testing to see when the difference
;; between two very small numbers fell below epsilon. But if these
;; numbers were very small, the difference might always lie below
;; epsilon - if epsilon were much smaller than the number we were
;; finding the square root of, for example.

(define (better-good-enough? guess old-guess)
  (< (abs (/ (- guess old-guess) guess)) epsilon))

(define (better-sqrt-iter guess old-guess x)
  (if (better-good-enough? guess old-guess)
      guess
      (better-sqrt-iter (improve guess x) guess x)))

(define (better-sqrt x)
  (better-sqrt-iter 1.0 0.5 x))

;; ========================================
;; Ex. 1.8
;; ========================================

(define (improve-cube guess x)
  (/ (+ (* guess 2) (/ x (square guess))) 3))

(define (cbrt-iter guess old-guess x)
  (if (better-good-enough? guess old-guess)
      guess
      (cbrt-iter (improve-cube guess x) guess x)))

(define (cbrt x)
  (cbrt-iter 1.0 0.5 x))

;; ========================================
;; Lexically Scoped Rewrite of sqrt
;; ========================================

(define (sqrtLex x)

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)

    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (sqrt-iter 1.0))
