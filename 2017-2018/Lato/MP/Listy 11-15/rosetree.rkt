#lang typed/racket

;; zad 6
(: prefixes (All (A) (-> (Listof A) (Listof (Listof A)))))
(define (prefixes xs)
  (if (null? xs)
      (list null)
      (cons null (map (lambda ([x : (Listof A)]) (cons (car xs) x)) (prefixes (cdr xs))))))

;; zad 7


(define-type Leaf 'leaf)
(define-type (Node A B) (List 'node A (Listof B)))
(define-type (Tree A) (U Leaf (Node A (Tree A))))

(define-predicate leaf? Leaf)
(define-predicate node? (Node Any Any))
(define-predicate tree? (Tree Any))

(: leaf Leaf)
(define leaf 'leaf)

(: node-val (All (A B) (-> (Node A B) A)))
(define (node-val x)
  (cadr x))

(: node-subtrees (All (A B) (-> (Node A B) (Listof B))))
(define (node-subtrees tree)
  (caddr tree))

(: make-node (All (A B) (-> A (Listof B) (Node A B))))
(define (make-node v nodes)
  (list 'node v nodes))

(: print-tree (All (A) (-> (Tree A) (Listof A))))
(define (print-tree tree)
  (if (leaf? tree)
      null
      (cons (node-val tree) (append-map (lambda([x : (Tree A)]) (print-tree x)) (node-subtrees tree)))))

(print-tree
   (make-node 5
              (list (make-node 3 (list 'leaf))
                    (make-node 2
                               (list (make-node 4 (list 'leaf))))
                    (make-node 1 (list leaf)))))