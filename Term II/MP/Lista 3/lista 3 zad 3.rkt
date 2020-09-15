#lang racket

;; ex. 3


;; helpers
(define (square x) (* x x))

(define (<= x y)
  (or (< x y) (= x y)))

;; points
(define (make-point x y)
  (cons x y))

(define (point? p)
  (pair? p))

(define (point-x p) (car p))

(define (point-y p) (cdr p))

(define (display-point p)
  (display "(")
  (display (point-x p))
  (display ", ")
  (display (point-y p))
  (display ")"))


;; vectors
(define (make-vect point dir len)
  (define (get-angle dir)
    (if (and (<= 0 dir) (< dir 6.28))
        dir
        (get-angle (- dir 6.28))))
  
  (list point (get-angle dir) len))

(define (vect-begin v)
  (car v))

(define (vect-dir v)
  (car (cdr v)))

(define (vect-len v)
  (car (cdr (cdr v))))

(define (vect-end v)
  (let ((x (* (vect-len v) (cos (vect-dir v))))
        (y (* (vect-len v) (sin (vect-dir v)))))
    (make-point x y)))

(define (vect-scale v k)
  (make-vect (vect-begin v) (vect-dir v) (* (vect-len v) k)))

(define (vect-translate v p)
  (let ([x (- (point-x p) (point-x (vect-begin v)))]
        [y (- (point-y p) (point-y (vect-begin v)))])
    (make-vect (make-point x y) (vect-dir v) (vect-len v))))

(define (display-vect v)
  (display "[")
  (display-point (vect-begin v))
  (display ", ")
  (display-point (vect-end v))
  (display "]"))



;; tests
(define p (make-point 0 0))
(define v (make-vect p 0 1))
(vect-end v)
(vect-scale v 2)
(display-vect (vect-translate v (make-point 3 4)))
