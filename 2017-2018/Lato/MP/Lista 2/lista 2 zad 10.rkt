#lang racket

(define (close-enough? a b)
    (< (abs (- a b)) 0.000001))

(define (square x) (* x x))

;; funkcja liczaca ulamki lancuchowe
(define (cont-frac N D)
  (define (find-f fn-1 An-1 An-2 Bn-1 Bn-2 n)
    (define An (+ (* (D n) An-1)
                  (* (N n) An-2)))
    (define Bn (+ (* (D n) Bn-1)
                  (* (N n) Bn-2)))
    (define fn (/ An Bn))
    
    (if (close-enough? fn-1 fn)
        fn
        (find-f fn An An-1 Bn Bn-1 (+ n 1))))

  (find-f 0 0 1 1 0 1))


;; atan
(define (atan-cf x)
  (/ x
     (+ 1.0
        (cont-frac
         (lambda (y) (square (* x y)))
         (lambda (y) (+ (* 2 y) 1))))))

;; tan
(define (tan-cf x)
  (/ x
     (+ 1.0
        (cont-frac
         (lambda (y) (- (square x)))
         (lambda (y) (+ (* 2 y) 1))))))

;; pi
(define (pi)
  (+ 3 (cont-frac (lambda (i) (square (- (* 2 i) 1.0)))
                  (lambda (i) 6.0))))


(cont-frac ( lambda (i) 1.0) ( lambda (i) 1.0)) ;; ulamek z zad. 6
(atan-cf 2)
(atan 2)
(tan-cf 2)
(tan 2)
(pi)

