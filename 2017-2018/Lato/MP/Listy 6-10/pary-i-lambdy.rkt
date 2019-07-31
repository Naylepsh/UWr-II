#lang racket

;; arithmetic expressions

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <=))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op-cons op args)
  (cons op args))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '=) =]
        [(eq? op '<) <]
        [(eq? op '<=) <=]
        [(eq? op '>) >]
        [(eq? op '>=) >=]))

;; lets

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

;; variables

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

;; null
(define (expr-null? e)
  (eq? e 'null))

(define (expr-null-pred? e)
  (and (list? e)
       (= (length e) 2)
       (eq? 'null? (car e))))

(define (null-pred-cons e)
  (list 'null? e))

(define (null-pred-expr e)
  (second e))

(define (null-cons) 'null)

;; booleans

(define expr-true #t)

(define expr-false #f)

(define (expr-boolean? e)
  (boolean? e))

;; some operators

;; and
(define (expr-and . args)
  (cons 'and args))

(define (expr-and-args e)
  (cdr e))

(define (expr-and? e)
  (and (list? e)
       (eq? 'and (car e))))

;; or
(define (expr-or . args)
  (cons 'or args))

(define (expr-or-args e)
  (cdr e))

(define (expr-or? e)
  (and (list? e)
       (eq? 'or (car e))))

;; ifs
(define (expr-if cond when-true when-false)
  (list 'if cond when-true when-false))

(define (expr-if-cond e)
  (second e))

(define (expr-if-when-true e)
  (third e))

(define (expr-if-when-false e)
  (fourth e))

(define (expr-if? e)
  (and (list? e)
       (= (length e) 4)
       (eq? 'if (first e))))

;; pairs

(define (exp-cons? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'cons)))

(define (cons-fst e)
  (second e))

(define (cons-snd e)
  (third e))

(define (cons-cons e1 e2)
  (list 'cons e1 e2))

(define (car? t)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'car)))

(define (car-expr e)
  (second e))

(define (cdr? t)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'cdr)))

(define (cdr-expr e)
  (second e))

;; pair?
(define (expr-pair-pred? e)
  (and (list? e)
       (= 2 (length e))
       (eq? 'pair? (car e))))

(define (expr-pair-pred-expr e)
  (second e))

;; lists
(define (expr-list->cons xs)
  (if (null? xs)
      (null-cons)
      (cons-cons (car xs) (expr-list->cons (cdr xs)))))

(define (expr-list? xs)
  (and (list? xs)
       (> (length xs) 0)
       (eq? 'list (car xs))))

(define (expr-list-args xs)
  (cdr xs))
        
;; lambdas

(define (lambda? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'lambda)
       (list? (cadr t))
       (andmap symbol? (cadr t))))

(define (lambda-vars e)
  (cadr e))

(define (lambda-expr e)
  (caddr e))

;; applications

(define (app? t)
  (and (list? t)
       (> (length t) 0)))

(define (app-proc e)
  (car e))

(define (app-args e)
  (cdr e))

;; expressions

(define (expr? t)
  (or (const? t)
      (and (op? t)
           (andmap expr? (op-args t)))
      (and (let? t)
           (expr? (let-expr t))
           (expr? (let-def-expr (let-def t))))
      (var? t)
      (and (exp-cons? t)
           (expr? (cons-fst t))
           (expr? (cons-snd t)))
      (and (car? t)
           (expr? (car-expr t)))
      (and (cdr? t)
           (expr? (cdr-expr t)))
      (and (lambda? t)
           (expr? (lambda-expr t)))
      (expr-null? t)
      (and (expr-null-pred? t)
           (expr? (null-pred-expr t)))
      (and (expr-pair-pred? t)
           (expr? (expr-pair-pred-expr t)))
      (and (app? t)
           (expr? (app-proc t))
           (andmap expr? (app-args t)))))

;; environments

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; closures

(define (closure-cons xs expr env)
  (list 'closure xs expr env))

(define (closure? c)
  (and (list? c)
       (= (length c) 4)
       (eq? (car c) 'closure)))

(define (closure-vars c)
  (cadr c))

(define (closure-expr c)
  (caddr c))

(define (closure-env c)
  (cadddr c))

;; evaluator

(define (eval-env e env)
  (cond [(expr-null? e) null]
        [(expr-null-pred? e) (null? (eval-env (null-pred-expr e) env))]
        [(expr-pair-pred? e) (pair? (eval-env (expr-pair-pred-expr e) env))]
        [(expr-list? e) (eval-env (expr-list->cons (expr-list-args e)) eval)]
        [(const? e) e]
        [(expr-boolean? e) e]
        [(op? e)
         (apply (op->proc (op-op e))
                (map (lambda (a) (eval-env a env))
                     (op-args e)))]
        [(expr-and? e) (andmap (lambda (exp) (eval-env exp env)) (expr-and-args e))]
        [(expr-or? e) (ormap (lambda (exp) (eval-env exp env)) (expr-or-args e))]
        [(let? e)
         (eval-env (let-expr e)
                   (env-for-let (let-def e) env))]
        
        [(var? e) (find-in-env (var-var e) env)]
        [(exp-cons? e)
         (cons (eval-env (cons-fst e) env)
               (eval-env (cons-snd e) env))]
        [(car? e)
         (car (eval-env (car-expr e) env))]
        [(cdr? e)
         (cdr (eval-env (cdr-expr e) env))]
        [(lambda? e)
         (closure-cons (lambda-vars e) (lambda-expr e) env)]
        [(expr-if? e)
         (if (eval-env (expr-if-cond e) env)
             (eval-env (expr-if-when-true e) env)
             (eval-env (expr-if-when-false e) env))]
        [(app? e)
         (apply-closure
           (eval-env (app-proc e) env)
           (map (lambda (a) (eval-env a env))
                (app-args e)))]))

(define (apply-closure c args)
  (eval-env (closure-expr c)
            (env-for-closure
              (closure-vars c)
              args
              (closure-env c))))

(define (env-for-closure xs vs env)
  (cond [(and (null? xs) (null? vs)) env]
        [(and (not (null? xs)) (not (null? vs)))
         (add-to-env
           (car xs)
           (car vs)
           (env-for-closure (cdr xs) (cdr vs) env))]
        [else (error "arity mismatch")]))

(define (env-for-let def env)
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))

(define (eval e)
  (eval-env e empty-env))

;; Cw. 3

;(define (pair? xs)
;  (and (cons? xs)
;       (
;(eval '(let (x 5) (lambda (z) (let (y 5) (+ x y z)))))
;(eval '(let (x 5) (lambda (x) (let (y 5) (+ x y)))))
;(eval '((lambda (x) (lambda (y) (+ x y))) 10))