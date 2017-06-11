;; ========================================
;; Exercises, Chapter 1.3, SICP
;; Formulating Abstractions with Higher-Order Procedures
;; ========================================

#lang sicp

;; Exercise 1.29

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
