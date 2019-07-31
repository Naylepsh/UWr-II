#lang racket

;; zad 3

(define ( btree? t)
  (or (eq? t 'leaf )
      ( and ( list? t)
            (= 4 ( length t))
            (eq? (car t) 'node )
            (btree? ( caddr t))
            (btree? ( cadddr t)))))

(define (mirror t)
  (if (btree? t)
      (if (eq? 'leaf t)
          'leaf
          (list (car t) (cadr t) (mirror (cadddr t)) (mirror (caddr t))))
      #f))

;; zrob testy -- tak srednio to dziala
(mirror '(node a (node b (node c leaf leaf) leaf) (node d leaf leaf)))      