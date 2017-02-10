;; ========================================
;; Exercises, Chapter 1.1, SICP
;; Elements of Programming
;; ========================================

#lang sicp

;; Ex 1.1

;; 10

;; (+ 5 3 4)

;; (- 9 1)

;; (/ 6 2)

;; (+ (* 2 4) (- 4 6))

;; (define a 3)

;; (define b (+ a 1))

;; (+ a b (* a b))

;; (= a b)

;; (if (and (> b a) (< b (* a b)))
;;     b
;;     a)

;; (cond ((= a 4) 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))

;; (+ 2 (if (> b a) b a))

;; (* (cond ((> a b) a)
;;          ((< a b) b)
;;          (else -1))
;;    (+ a 1))

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

;; ========================================
;; Exercise 1.9
;; ========================================

;; First implementation (linear recursive process:

;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; Second implementation (linear iterative process:

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;; ========================================
;; Exercise 1.10
;; ========================================

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024


;; (A 2 4)
;; => 65536

;; (A 3 3)
;; => 65536

(define (f n) (A 0 n))
;; f n = 2*n

(define (g n) (A 1 n))
;; g n = 2^n

;; (A 2 5)
;; (A 1 (A 2 4))
;; (A 1 (A 1 (A 2 3)))
;; (A 1 (A 1 (A 1 (A 2 2))))
;; (A 1 (A 1 (A 1 (A 1 (A 2 1)))))
;; (A 1 (A 1 (A 1 (A 1 2))))
;; (A 1 (A 1 (A 1 (A 0 (A 1 1)))))
;; (A 1 (A 1 (A 1 (A 0 2))))
;; (A 1 (A 1 (A 1 4)))
;; (A 1 (A 1 (A 0 (A 1 3))))
;; (A 1 (A 1 (A 0 (A 0 (A 1 2)))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 (A 1 1))))))
;; (A 1 (A 1 (A 0 (A 0 (A 0 2)))))
;; (A 1 (A 1 (A 0 (A 0 4))))
;; (A 1 (A 1 (A 0 8)))
;; (A 1 (A 1 16))
;; (A 1 (A 0 (A 1 15)))
;; (A 1 (A 0 (A 0 (A 1 14))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 13)))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 1 12))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11)))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9)))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7)))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5)))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4))))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3)))))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1)))))))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4)))))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8))))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16)))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32))))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64)))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128))))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256)))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512))))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024)))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 (A 0 2048))))))
;; (A 1 (A 0 (A 0 (A 0 (A 0 4096)))))
;; (A 1 (A 0 (A 0 (A 0 8192))))
;; (A 1 (A 0 (A 0 16384)))
;; (A 1 (A 0 32768))
;; (A 1 65536)

(define (h n) (A 2 n))
;; h n = 2^(g n) = 2^(2^n)

;; ========================================
;; Exercise 1.11
;; ========================================

;; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3)
;; a <- a + 2b + 3c
;; b <- a
;; c <- b

(define (f-iter n)
  (f-iter-help 2 1 0 n))

(define (f-iter-help a b c idx)
  (cond ((= idx 0) c)
        ((= idx 1) b)
        ((= idx 2) a)
        (else
         (f-iter-help (+ a (* 2 b) (* 3 c)) a b (- idx 1)))))

;; ========================================
;; Exercise 1.12
;; ========================================

(define (pascal n)
  (if (= n 1)
      '((1))
      (cons (pascals-row n) (pascal (- n 1)))))

(define (pascals-row n)
  (pascals-row-iter n 0))

(define (pascals-row-iter n k)
  (if (= n k)
      '(1)
      (cons (pascals-rule n k) (pascals-row-iter n (+ k 1)))))

(define (pascals-rule n k)
  (cond ((= n k) 1)
        ((= k 0) 1)
        (else
         (+ (pascals-rule (- n 1) (- n k))
            (pascals-rule (- n 1) k)))))

;; ========================================
;; Exercise 1.14
;; ========================================

;; The `count-change' procedure is tree recursive and thus executes in
;; exponential time and linear stack space.

;; ========================================
;; Exercise 1.15
;; ========================================

(define (cube x) (* x x x))

(define (series x) (- (* 3 x) (* 4 (cube x))))

(define (sine theta)
  (if (not (> (abs theta) 0.1))
      theta
      (series (sine (/ theta 3.0)))))

;; The function `series' is called by `sine' five times :

;; (sine 12.15)
;; ...
;; (series (sine 4.05))
;; ...
;; (series (sine 1.42))
;; ...
;; (series (sine 0.44))
;; ...
;; (series (sine 0.14))
;; ...
;; (series (sine 0.04))

;; The function is a linear recursive procedure, so it in linear stack
;; space. It 's running time can be derived using the Master Method, where
;; T(n) <= T(n/3) + O(n), which implies O(n) time complexity.

;; ========================================
;; Exercise 1.16
;; ========================================

(define (fast-expt-iter base n acc)
  (cond ((= n 0) acc)
        ((even? n) (fast-expt-iter (square base) (/ n 2) acc))
        (else (fast-expt-iter base (- n 1) (* base acc)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

;; ========================================
;; Exercise 1.17
;; ========================================

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- b 1))))))

;; ========================================
;; Exercise 1.18
;; ========================================

(define (fast-mul-iter a b acc)
  (cond ((= b 0) acc)
        ((even? b) (fast-mul-iter (* a 2) (/ b 2) acc))
        (else (fast-mul-iter a (- b 1) (+ a acc)))))

;; ========================================
;; Exercise 1.19
;; ========================================

(define (sqr x)
  (expt x 2))

(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

(define (fast-fib-iter a b p q counter)
  (cond ((= counter 0) b)
        ((even? counter)
         (fast-fib-iter a
                        b
                        (+ (sqr p) (sqr q))
                        (+ (sqr q) (* 2 p q))
                        (/ counter 2)))
        (else (fast-fib-iter (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- counter 1)))))

(define (fast-fib n)
  (fast-fib-iter 1 0 0 1 n))
