#lang racket

;; ex. 2

;; helpers
(define (square x) (* x x))

;; points
(define (make-point x y)
  (cons x y))

(define (point? p)
  (pair? p))

(define (point-x p) (car p))

(define (point-y p) (cdr p))


;; vectors
(define (make-vect p1 p2)
  (if (and (pair? p1) (pair? p2))
      (cons p1 p2)
      #f))

(define (vect? v)
  (and (pair? (car v)) (pair? (cdr v))))

(define (vect-begin v) (car v))

(define (vect-end v) (cdr v))

(define (vect-length v)
  (let ([x (- (point-x (vect-begin v)) (point-x (vect-end v)))]
        [y (- (point-y (vect-begin v)) (point-y (vect-end v)))])
    (sqrt (+ (square x) (square y)))))

(define (vect-scale v k)
  (let ([x (+ (point-x (vect-begin v))
              (* (- (point-x (vect-end v)) (point-x (vect-begin v)))  k))]
        [y (+ (point-y (vect-begin v))
              (* (- (point-y (vect-end v)) (point-y (vect-begin v))) k))])
    (make-vect (vect-begin v) (make-point x y))))

(define (vect-translate v p)
  (let ([x (- (point-x p) (point-x (vect-begin v)))]
        [y (- (point-y p) (point-y (vect-begin v)))])
    (make-vect p (make-point (+ (point-x (vect-end v)) (point-x p))
                             (+ (point-y (vect-end v)) (point-y p))))))



(define (display-point p)
  (display "(")
  (display (point-x p))
  (display ", ")
  (display (point-y p))
  (display ")"))

(define (display-vect v)
  (display "[")
  (display-point (vect-begin v))
  (display ", ")
  (display-point (vect-end v))
  (display "]"))


(define p1 (make-point 1 2))
(define p2 (make-point 4 6))
(define v (make-vect p1 p2))
(define v2 (vect-scale v 2))
(vect-length v)
(vect-length v2)

