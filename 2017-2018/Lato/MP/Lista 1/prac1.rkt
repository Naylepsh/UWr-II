#lang racket

(define (dist x y)
  (abs(- x y)))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (average-of-3 x y z)
  (/ (+ x y z) 3))

(define (cube-root x)
  ;; local functions
  (define (improve approx)
    (average-of-3 (/ x (square approx)) (* 2 approx) 0))
  (define (good-enough? approx)
    (< (dist x (cube approx)) 0.0001))
  (define (iter approx)
    (cond
      [(good-enough? approx) approx]
      [else                  (iter (improve approx))]))

  (iter 1.0))


;; tests
(cube-root 1)
(cube-root 2)
(cube-root 27)
(cube-root -8)
(cube-root 125)
(cube-root 512)
