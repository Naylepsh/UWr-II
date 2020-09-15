#lang racket

;; ex. 4

;; recursion
(define (reverse-rec items)
  (if (null? items) ;; (val null)
      items
      (append (reverse-rec (cdr items))
              (list (car items)))))

;; iteration
(define (reverse-iter items)
  (define (iter List acc)
    (if (null? List)
        acc
        (iter (cdr List) (cons (car List) acc))))

  (iter items null))


(define L (list 1 2 3 4 5))
(reverse-rec L)
(reverse-iter L)

(define M null)
(reverse-rec M)
      