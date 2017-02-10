;; ==================================================
;; SICP, Chapter 1-1 - ELements of Programming
;; ==================================================

(ns elements)

;; Defining New Functions

(defn square [x]
  (* x x))

(defn spread1 [x y z]
  (- (max x y z) (min x y z)))

(defn spread2
  ([x] 0)
  ([x y] (- (max x y) (min x y)))
  ([x y z] (- (max x y z) (min x y z))))

(defn spread3 [& nums]
  (- (apply max nums) (apply min nums)))

;; Substitution Model

(defn sum-squares [x y]
  (+ (square x) (square y)))

(defn f [a]
  (sum-squares (* a 1) (* a 2)))

;; Newton's Method

(defn average [x y]
  (/ (+ x y) 2))

(defn abs [n]
  (max n (- n)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

