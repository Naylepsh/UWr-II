#lang racket

(define (lcons x f)
  (cons x f))

(define (lhead l)
  (car l))

(define (ltail l)
  ((cdr l)))

(define (nats-from m)
  (lcons
   m
   (lambda () (nats-from (+ m 1)))))

(define nats
  (nats-from 0))

(define (take n l)
  (if (or (null? l) (= n 0))
      null
      (cons (lhead l)
            (take (- n 1) (ltail l)))))

(define (filter p l)
  (cond [(null? l) null]
        [(p (lhead l))
         (lcons (lhead l)
                (lambda ()
                  (filter p (ltail l))))]
        [else (filter p (ltail l))]))

(define (prime? n)
  (define (div-by m)
    (cond [(= m n) true]
          [(= (modulo n m) 0) false]
          [else (div-by (+ m 1))]))
  (if (< n 2)
      false
      (div-by 2)))

(define (fib-from n)
  (define (iter a b)
    (lcons (+ a b) (lambda () (iter b (+ a b)))))
  (cond
    [(= n 0) (lcons 0 (lambda () (fib-from (+ n 1))))]
    [(= n 1) (lcons 1 (lambda () (fib-from (+ n 1))))]
    [else (iter 0 1)]))

(define fib (fib-from 0))


(define (ints-from n)
  (if (= (modulo n 2) 0)
      (lcons (/ n (- 2)) (lambda () (ints-from (+ n 1))))
      (lcons (/ (+ n 1) 2) (lambda () (ints-from (+ n 1))))))

(define ints (ints-from 0))





