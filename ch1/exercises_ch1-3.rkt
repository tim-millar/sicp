;; ========================================
;; Exercises, Chapter 1.3, SICP
;; Formulating Abstractions with Higher-Order Procedures
;; ========================================

#lang sicp

;; ========================================
;; Exercise 1.29
;; ========================================

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n)
  (+ n 1))

(define (cube x)
  (* x x x))

(define (simpsons f a b n)

  (define h (/ (- b a) n))

  (define (y k) (f (+ a (* k h))))

  (define (coeff i)
    (cond ((or (= i 0) (= i n)) 1)
          ((odd? i) 4)
          ((even? i) 2)))

  (define (term k)
    (* (coeff k) (y k)))

  (* (/ h 3.0) (sum term a inc n)))

;; > (simpsons cube 0 1 100.0)
;; 0.24999999999999992
;; > (simpsons cube 0 1 1000.0)
;; 0.2500000000000003


;; ========================================
;; Exercise 1.30
;; ========================================

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; ========================================
;; Exercise 1.31
;; ========================================

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (add-two n)
  (+ 2 n))

(define num
  (* (product identity 2 add-two 32)
     (product identity 4 add-two 34)))

(define denom
  (* (product identity 3 add-two 33)
     (product identity 3 add-two 33)))

(define pi-star
  (* 4 (exact->inexact (/ num denom))))

(define (iter-prod term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;; ========================================
;; Exercise 1.32
;; ========================================

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

(define (sum-iter-acc term a next b)
  (iter-accumulate + 0 term a next b))

(define (prod-iter-acc term a next b)
  (iter-accumulate * 1 term a next b))

;; ========================================
;; Exercise 1.33
;; ========================================

(define (filtered-accumulate predicate combiner null-value term a next b)
  (cond ((> a b)
         null-value)
        ((predicate a)
         (combiner (term a)
                   (filtered-accumulate predicate combiner null-value term (next a) next b)))
        (else
         (filtered-accumulate predicate combiner null-value term (next a) next b))))

(define (divides? a b)
   (= (remainder b a) 0))

(define (square x)
   (* x x))

(define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
   (find-divisor n 2))

(define (prime? n)
   (= n (smallest-divisor n)))

(define (square-primes a b)
  (filtered-accumulate prime? + 0 identity a inc b))

(define (gcd a b)
   (if (= b 0)
       a
       (gcd b (remainder a b))))

(define (positive-primes-product n)
  (define (relatively-prime? i) (= (gcd i n) 1))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc n))

;; ========================================
;; Exercise 1.34
;; ========================================

(define (f g)
  (g 2))

;; Suppose you apply f to itself -- i.e., (f f). The function will error. We
;; can see this through substitution:

;; (f f)
;; (f 2)
;; (2 2)

;; `2' is not a function, so cannot be applied to any arguments, hence the error.

;; ========================================
;; Exercise 1.35
;; ========================================

;; x = 1 + 1/x
;; <=>
;; x^2 = x + 1
;; <=>
;; x^2 - x - 1 = 0
;;
;; using quadratic formula, roots of above are:
;; x1 = (1 + sqrt(5))/2
;; x2 = (1 - sqrt(5))/2

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;; 1.6180327868852458

;; (/ (+ 1 (sqrt 5)) 2)
;; 1.618033988749895

;; ========================================
;; Exercise 1.36
;; ========================================

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          ((newline)
           (display guess)
           (try next)))))
  (try first-guess))

;; (fixed-point-print (lambda (x) (/ (log 1000) (log x))) 1.01)

(define (average x y)
  (/ (+ x y) 2.0))

(define (fixed-point-print-avg f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          ((newline)
           (display guess)
           (try (average guess next))))))
  (try first-guess))

;; (fixed-point-print-avg (lambda (x) (/ (log 1000) (log x))) 1.01)

;; Both procedures converge. Without damping, procedure repeats for about 40
;; iterations; with damping procedure repeats for about 15.

;; ========================================
;; Exercise 1.37
;; ========================================

(define (cont-frac n d k)
  (if (= k 0)
      (/ (n k) (d k))
      (/ (n k)
         (+ (d k)
            (cont-frac n d (- k 1))))))

;; [phi] ~= 1.6180
;; 1 / [phi] ~= 0.618047
;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
;; => 0.6180257510729613 ~= 0.6180

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))
  (iter k 0))
