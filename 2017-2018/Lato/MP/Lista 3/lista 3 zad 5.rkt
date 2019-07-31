#lang racket

;; ex. 5

(define (insert L val)
  (cond
    ((null? L)       (list val))
    ((> (car L) val) (cons val L))
    (else (cons (car L) (insert (cdr L) val)))))



(define (insert-sort L)
  (define (iter L sorted-list)
    (if (null? L)
        sorted-list
        (iter (cdr L) (insert sorted-list (car L)))))

  (iter L null))

;; rekursja ogonowa
(define (sort xs)
    (if (null? xs)
        null
        (insert (sort (cdr xs)) (car xs))))