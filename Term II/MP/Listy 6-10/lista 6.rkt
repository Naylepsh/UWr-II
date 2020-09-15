#lang racket

;; arithmetic expressions

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (arith-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith-expr? (binop-left  t))
           (arith-expr? (binop-right t)))))

;; ex. 1
#|
(define (arith->rpn xs)
  (define (rec xs)
    (cond
      [(null? xs) null]
      [(const? xs) xs]
      [(binop? xs) (list (rec (binop-left xs)) (rec (binop-right xs)) (binop-op xs))]
      [else (error "Invalid expression")]))
  (flatten (rec xs)))|#

;; v2
(define (arith->rpn exp)
  (define (rec e acc)
    (cond
      [(const? e) (cons e acc)]
      [(binop? e) (rec (binop-left e)
                       (rec (binop-right e)
                            (cons (binop-op e)
                                   acc)))]
      [else (error "Invalid expression")]))
  (rec exp null))

;; ex. 2 ;; (provide (all-define-out)) ;; zamknac to w jednym pliku
;; (require "stack.rkt")
(define (stack? xs)
  (list? xs))

(define push cons)

(define (pop stack)
  stack)

;; ex. 3
(define (op? e)
  (member e'(+ - * /)))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (eval-rpn exp)
  (define (helper e val-stack)
    (cond
      [(null? e)        (car val-stack)]
      [(number? (car e)) (helper (cdr e) (push (car e) val-stack))]
      [(op? (car e))
       (let ([a (car (pop val-stack))]
             [b (car (pop (cdr (pop val-stack))))])
         (helper (cdr e)
                 (push ((op->proc (car e)) b a)
                       (cdr (pop (cdr (pop val-stack)))))))]))
  (helper exp null))

(define exp (arith->rpn '(/ (+ (/ (+ 2 7) 3) (* (- 14 3) 4)) 2)))
;'(2 7 + 3 / 14 3 - 4 * + 2 /)
(eval-rpn exp)
(eval-rpn '(1 2 4 + + 6 /))