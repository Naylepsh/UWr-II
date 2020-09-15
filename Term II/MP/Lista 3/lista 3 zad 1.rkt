#lang racket

;; ex. 1

(define (make-rat x y)
  (if (and (integer? x)
           (integer? y)
           (not (= y 0)))
      (let ([a (/ x (gcd x y))]
            [b (/ y (gcd x y))])
        (cons a (cons b null)))
      (display "Error")))

(define (rat-num r)
  (car r))

(define (rat-den r)
  (car (cdr r)))

(define (rat? r)
  (and (list? r)
       (integer? (car r))          ;; x is an integer
       (not (null? (car (cdr r)))) ;; y is not null
       (integer? (car (cdr r)))    ;; y is an integer
       (not (= 0 (car (cdr r))))   ;; y != 0
       (null? (cdr (cdr r)))))     ;; list has only two elements