#lang racket

(require racket/contract)

;; pow
(define/contract (pow a n)
  (-> real? integer? real?)
  (if (= n 0)
      1
      (* a (pow a (- n 1)))))

(define negative-natural/c (and/c integer? (not/c positive?)))

(define/contract (foo x)
  (-> negative-natural/c integer?)
  (abs x))


(define/contract (use-fun f x)
  (-> (-> integer? integer?) integer? boolean?)
  (= (modulo (f x) 2) 0))

(define/contract (my-map f xs)
  (let ([a (new-∀/c 'a)]
        [b (new-∀/c 'b)])
    (-> (-> a b) (listof a) (listof b)))
  (if (null? xs)
      xs
      (append (list (f (car xs))) (my-map f (cdr xs)))))

(define/contract (len xs)
  (->i ([xs list?])
       [result (and/c integer? positive?)]
       #:post (xs result)
       (= result (length xs)))
  (if (null? xs)
      0
      (+ 1 (len (cdr xs)))))

;; Lista 12 -- zadania
;; zad 1
(define/contract (suffixes xs)
  (let ([a (new-∀/c 'a)])
        (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
      (append (list xs) (suffixes (cdr xs)))))

;; zad 2 w sqrt.rkt

;; zad 3 -- KONTRAKT ZALEZNY
(define/contract (my-filter p xs)
  (and/c (let ([a (new-∀/c 'a)])
           (-> (-> a boolean?) (listof a) (listof a)))
         (->i ([p (-> any/c boolean?)]
               [xs (listof any/c)])
              [result (listof any/c)]
              #:post (result p)
              (andmap p result)))
  (cond
    [(null? xs) null]
    [(p (car xs)) (cons (car xs) (my-filter p (cdr xs)))]
    [else         (my-filter p (cdr xs))]))


;; zad 4
( define-signature monoid^
   (( contracted
      [ elem? (-> any/c boolean?)]
      [ neutral elem?]
      [ oper (-> elem? elem? elem?) ])))

(define-unit monoid-int@ ;; monoid-list
  (import)
  (export monoid^)

  (define elem? integer?) ;; dla listy (define elem? list)
  (define neutral 0)      ;; (define neutral null)
  (define oper +))        ;; (define oper append)

(define-values/invoke-unit/infer monoid-int@)
;(oper 1 2)

(require quickcheck)

(quickcheck
 (property
  ([x arbitrary-integer] ;; dla list: [xs arbitrary-list]
   [y arbitrary-integer]
   [z arbitrary-integer])
  (and (and (= (oper neutral x) x) (= (oper x neutral) x)) ;; (equal? (oper xs neutral) xs)
       (= (oper (oper x y) z) ;; (equal? (oper (oper xs ys) zs) (oper xs (oper ys zs)))
          (oper x (oper y z))))))
