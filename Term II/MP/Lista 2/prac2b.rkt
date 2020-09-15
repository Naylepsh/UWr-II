#lang racket

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (dist x y)
  (abs (- x y)))

(define (close-enough? x y)
  (< (dist x y) 0.00001))

(define (fix-point f x0)
  (let ((x1 (f x0)))
    (if (close-enough? x0 x1)
        x0
        (fix-point f x1))))

(define (identity x) x)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated p n)
  (if (< n 1)
      identity
      (compose p (repeated p (- n 1)))))


;; funkcja obliczajaca n-ty pierwiastek z x
;; nie zadziala dla n <= 0
(define (nth-root x n)
  (fix-point ((repeated average-damp (log n 2))
              (lambda (y) (/ x (expt y (- n 1)))))
             1.0))


(nth-root -8 3)
(nth-root 1 7)
(nth-root 2 3)
(nth-root 4 2)
(nth-root 27 3)
(nth-root 256 4)
(nth-root 3215 5)
(nth-root 11111111 6)

  