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
;; Exercise 1.33
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
