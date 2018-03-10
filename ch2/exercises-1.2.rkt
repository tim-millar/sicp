;; ========================================
;; Exercises, Chapter 2.2, SICP
;; Data Abstraction
;; ========================================

#lang sicp

;; ========================================
;; Exercise 2.1
;; ========================================

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((or (and (< n 0) (< d 0)) (< d 0))
           (cons (* (- 1) (/ n g)) (* (- 1) (/ d g))))
          (else
           (cons (/ n g) (/ d g))))))

;; (define (make-rat n d)
;;   (let ((g (gcd n d)))
;;     (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

;; ========================================
;; Exercise 2.2
;; ========================================

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)

  (let ((x1 (x-point (start-segment segment)))
        (y1 (y-point (start-segment segment)))
        (x2 (x-point (end-segment segment)))
        (y2 (y-point (end-segment segment))))

    (make-point (+ x1
                   (/ (- x2 x1) 2.0))
                (+ y1
                   (/ (- y2 y1) 2.0)))))

;; ========================================
;; Exercise 2.3
;; ========================================

(define (square n)
  (* n n))

(define (point-dist p1 p2)
  (let ((x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
    (sqrt (+ (square (- x2 x1))
             (square (- y2 y1))))))

(define (seg-length seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (point-dist p1 p2)))

(define (make-rectangle seg-x seg-y)
  (cons seg-x seg-y))

(define (seg-x rectangle)
  (car rectangle))

(define (seg-y rectangle)
  (cdr rectangle))

;; interface implemented by rectangle
(define (width rect)
  (seg-length (seg-x rect)))

(define (height rect)
  (seg-length (seg-y rect)))

;; top level functions
(define (area rectangle)
    (* (width rectangle) (height rectangle)))

(define (perimeter rectangle)
  (* (+ (width rectangle) (height rectangle)) 2))

(define p1 (make-point 1 3))
(define p2 (make-point 2 1))
(define p3 (make-point 5 3))

(define seg1 (make-segment p1 p2))
(define seg2 (make-segment p2 p3))

(define rec1 (make-rectangle seg1 seg2))

;; Any data abstraction that implements height and width functions can be used
;; by the area and perimeter functions. Unfortunately scheme does not seem to
;; allow overriding, so this is not actually possible here.

;; As an example of a possible laternative, consider an implementation of the
;; rectangle that includes a bottom-left point, an angle of rotation, and a
;; height and a width. Then hight and width functions could be created that
;; returned those attributes. Then we could use the same area and perimeter
;; implementations.

;; E.g.,

;; (define (rectangle point angl height width)
;;   (cons (cons point angl) (cons height width)))

;; (define (height rectangle)
;;   (cdr (car rectangle)))

;; (define (width rectangle)
;;   (cdr (cdr rectangle)))

;; ========================================
;; Exercise 2.4
;; ========================================

(define (f/cons x y)
  (lambda (m) (m x y)))

(define (f/car z)
  (z (lambda (p q) p)))

(define (f/cdr z)
  (z (lambda (p q) q)))

;; (car z)
;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

;; ========================================
;; Exercise 2.5
;; ========================================

(define (n/cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (n/divides? a b)
  (= 0 (remainder b a)))

(define (n/divide-iter x n)
  (cond ((= x 0) x)
        ((not (n/divides? n x)) x)
        (else (n/divide-iter (/ x n) n))))

(define (n/cdr-helper pr)
  (n/divide-iter pr 2))

(define (n/car-helper pr)
  (/ pr (n/cdr-helper pr)))

(define (n/cdr pr)
  (log (n/cdr-helper pr) 3))

(define (n/car pr)
  (log (n/car-helper pr) 2))

;; exercises-1.2.rkt﻿> (define p1 (n/cons 3 2))
;; exercises-1.2.rkt﻿> (n/car p1)
;; 3.0
;; exercises-1.2.rkt﻿> (n/cdr p1)
;; 2.0
;; exercises-1.2.rkt﻿> 

;; ========================================
;; Exercise 2.6
;; ========================================

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define zero
  (lambda (f)
    (lambda (x) x)))

(define one
  (lambda (f)
    (lambda (x)
      (f (((lambda (f) (lambda (x) x)) f) x)))))

(define two
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x)
               (f (((lambda (f) (lambda (x) x)) f) x)))) f) x)))))

(define (+ m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

;; ========================================
;; Exercise 2.7
;; ========================================

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; ========================================
;; Exercise 2.8
;; ========================================

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; ========================================
;; Exercise 2.9
;; ========================================

(define (width-interval x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

;; (width-interval (add-interval x y))
;; (width-interval (cons (+ (lower-bound x) (lower-bound y))
;;                       (+ (upper-bound x) (upper-bound y))))
;; (/ (+ (+ (lower-bound x) (lower-bound y))
;;       (+ (upper-bound x) (upper-bound y)))
;;    2)
;; (+ (/ (+ (lower-bound x) (upper-bound x)) 2)
;;    (/ (+ (lower-bound y) (upper-bound y)) 2))

;; ========================================
;; Exercise 2.10
;; ========================================

(define (div-interval-no-zero x y)
  (cond ((= (lower-bound y) (upper-bound y))
         (error 'division-by-zero))
        (else
         (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                        (/ 1.0 (lower-bound y)))))))

;; ========================================
;; Exercise 2.11
;; ========================================

(define (reduce func lst)
  (if (null? (cdr lst))
      (car lst)
      (func (car lst) (reduce func (cdr lst)))))

(define (all? prd lst)
  (reduce (lambda (x y) (and x y)) (map prd lst)))

(define (mul-interval-signs x y)
  (let ((lower-x (lower-bound x))
        (upper-x (upper-bound x))
        (lower-y (lower-bound y))
        (upper-y (upper-bound y)))
    (cond ((all? negative? '(lower-x upper-x lower-y upper-y))
           (make-interval (* lower-x lower-y) (* upper-x upper-y)))
          ((all? negative? '(lower-x upper-x lower-y))
           (make-interval (* lower-x upper-y) (* lower-x lower-y)))
          ((all? negative? '(lower-x lower-y upper-y))
           (make-interval (* upper-x lower-y) (* lower-x lower-y)))
          ((all? negative? '(lower-x lower-y))
           (make-interval (min (* lower-x upper-y) (* upper-x lower-y))
                          (* upper-x upper-y)))
          ((all? negative? '(lower-y upper-y))
           (make-interval (* upper-x lower-y) (* lower-x upper-y)))
          ((all? negative? '(lower-x upper-x))
           (make-interval (* lower-x upper-y) (* upper-x lower-y)))
          ((negative? lower-y)
           (make-interval (* upper-x lower-y) (* upper-x upper-y)))
          ((negative? lower-x)
           (make-interval (* lower-x upper-y) (* upper-x upper-y)))
          (else
           (make-interval (* lower-x lower-y) (* upper-x upper-y))))))
