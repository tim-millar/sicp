;; ========================================
;; Exercises, Chapter 1.3, SICP
;; Formulating Abstractions with Higher-Order Procedures
;; ========================================

#lang sicp

;; Exercise 1.29

(define (integral f a b n)

  (define h (/ (- b a) n))

  (define (coeff i)
    (cond ((= i 0) 1)
          ((= i n) 1)
          ((= (modulo i 2) 0) 4)
          (else 2)))

  (define (y i)
    (f (+ a (* i h))))

  (define (sigma f i n)
    (if (= i n)
         (f n)
         (+ (f i) (sigma f (+ i 1) n))))

  (define (g x)
    (* (coeff x) (y x)))

  (* (/ h 3) (sigma g 0 n)))

(define (cube x)
  (* x x x))

;; > (integral cube 0 1 100.0)
;; 0.24671666666666678
;; > (integral cube 0 1 100000.0)
;; 0.249996666716668
