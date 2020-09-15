#lang racket

;; zad 4
(define (flatten tree ans)
  (if (eq? tree 'leaf)
      ans
      (flatten (caddr tree) (cons (cadr tree) (flatten (cadddr tree) ans)))))


;;; drzewa binarne

(define (leaf? x)
  (eq? x 'leaf))

(define leaf 'leaf)

(define (node? x)
  (and (list? x)
       (= (length x) 4)
       (eq? (car x) 'node)))

(define (node-val x)
  (cadr x))

(define (node-left x)
  (caddr x))

(define (node-right x)
  (cadddr x))

(define (make-node v l r)
  (list 'node v l r))

(define (tree? t)
  (or (leaf? t)
      (and (node? t)
           (tree? (node-left t))
           (tree? (node-right t)))))

(define (bst-insert x t)
  (cond [(leaf? t)
         (make-node x leaf leaf)]
        [(< x (node-val t))
         (make-node (node-val t)
                    (bst-insert x (node-left t))
                    (node-right t))]
        [else
         (make-node (node-val t)
                    (node-left t)
                    (bst-insert x (node-right t)))]))


;; zad 5
(define (tree-sort l t)
  (if (null? l)
      (flatten l null)
      (cons (cdr l) (bst-insert (car l) t))))


;; jakies losowe
(define (random-list k)
  (if (= k 0)
      null
      (cons (random 500) (random-list (- k 1)))))



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