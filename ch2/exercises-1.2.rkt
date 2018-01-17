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
