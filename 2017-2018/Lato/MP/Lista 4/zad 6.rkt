#lang racket

;; zad 6
(define (delete tree x)
  (cond
    [(null? tree ) null]
    [(= x (node-val tree))
     (if (leaf? (node-right tree))
         (node-left tree)
         (let ([m (bst-min (node-right tree))])
           (make-node m (node-left tree) (delete (node-right tree) m))))]
    [(> x (node-val tree))
     (make-node (node-val tree) (node-left t) (delete (node-right tree) x))]
    [else ;(< x (node-val tree))
     (make-node (node-val tree) (delete (node-left tree) x) (node-right tree))]))

(define (bst-min tree)
  (if (eq? 'leaf (node-val tree))
      (node-val tree)
      (bst-min (node-left tree))))