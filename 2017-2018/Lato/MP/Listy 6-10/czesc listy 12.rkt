#lang racket

(require racket/contract)

(define/contract (suffixes xs)
  (let ([a (new-∀/c 'a)])
    (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
      (append (list xs) (suffixes (cdr xs)))))

(define (prefixes xs)
  (let ([a (new-∀/c 'a)])
    (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
      (cons null (map (lambda (x) (cons (car xs) x)) (prefixes (cdr xs))))))

(define (filter p? xs)
  (if (null? xs)
      null
      (if (p? (car xs))
          (cons (car xs) (filter p? (cdr xs)))
          (filter p? (cdr xs)))))

(define/contract (filter-c p? xs)
  (let ([a (new-∀/c 'a)])
    (-> (-> a boolean?) (listof a) (listof a)))
  (if (null? xs)
      null
      (if (p? (car xs))
          (cons (car xs) (filter-c p? (cdr xs)))
          (filter-c p? (cdr xs)))))

(define/contract (filter-c2 p? xs)
  (and/c (let ([a (new-∀/c 'a)])
           (-> (-> a boolean?) (listof a) (listof a)))
         (->i ([p? (-> any/c boolean?)]
               [xs (listof any/c)])
               [result (listof any/c)]
               #:post (result p?)
               (andmap p? result)))
  (if (null? xs)
      null
      (if (p? (car xs))
          (cons (car xs) (filter-c2 p? (cdr xs)))
          (filter-c2 p? (cdr xs)))))

(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))))

(define/contract (map-c f xs)
  (and/c (let ([a (new-∀/c 'a)]
               [b (new-∀/c 'b)])
           (-> (-> a b) (listof a) (listof b)))
         (->i ([f (-> any/c any/c)]
               [xs (listof any/c)])
              [result (listof any/c)]
              #:post (result xs)
              (= (length result) (length xs))))
  (if (null? xs)
      null
      (cons (f (car xs)) (map-c f (cdr xs)))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))
