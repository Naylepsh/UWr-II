#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości

(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;; self-evaluating expressions

(define (const? t)
  (or (number? t)
      (my-symbol? t)
      (eq? t 'true)
      (eq? t 'false)))

;; arithmetic expressions

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <= eq?))))

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
        [(eq? op '=)  (compose bool->val =)]
        [(eq? op '>)  (compose bool->val >)]
        [(eq? op '>=) (compose bool->val >=)]
        [(eq? op '<)  (compose bool->val <)]
        [(eq? op '<=) (compose bool->val <=)]
        [(eq? op 'eq?) (lambda (x y)
                         (bool->val (eq? (symbol-symbol x)
                                         (symbol-symbol y))))]))

;; symbols

(define (my-symbol? e)
  (and (tagged-tuple? 'quote 2 e)
       (symbol? (second e))))

(define (symbol-symbol e)
  (second e))

(define (symbol-cons s)
  (list 'quote s))

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
  (and (tagged-tuple? 'let 3 t)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

;; lazy-let
(define (lazy-let? t)
  (and (tagged-tuple? 'lazy-let 3 t)
       (let-def? (cadr t))))

;; variables

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

;; pairs

(define (cons? t)
  (tagged-tuple? 'cons 3 t))

(define (cons-fst e)
  (second e))

(define (cons-snd e)
  (third e))

(define (cons-cons e1 e2)
  (list 'cons e1 e2))

(define (car? t)
  (tagged-tuple? 'car 2 t))

(define (car-expr e)
  (second e))

(define (cdr? t)
  (tagged-tuple? 'cdr 2 t))

(define (cdr-expr e)
  (second e))

(define (pair?? t)
  (tagged-tuple? 'pair? 2 t))

(define (pair?-expr e)
  (second e))

(define (pair?-cons e)
  (list 'pair? e))


;; if

(define (if? t)
  (tagged-tuple? 'if 4 t))

(define (if-cons b t f)
  (list 'let b t f))

(define (if-cond e)
  (second e))

(define (if-then e)
  (third e))

(define (if-else e)
  (fourth e))

;; and
(define (expr-and? expr)
  (and (list? expr)
       (eq? 'and (first expr))))

(define (expr-and-eval expr env)
  (cond
    [(null? expr) 'true]
    [(eq? #t (val->bool (eval-env (car expr) env))) (expr-and-eval (cdr expr) env)]
    [else 'false]))

;; or
(define (expr-or? expr)
  (and (list? expr)
       (eq? 'or (first expr))))

(define (expr-or-eval expr env)
  (cond
    [(null? expr) 'false]
    [(eq? #t (val->bool (eval-env (car expr) env))) 'true]
    [else (expr-or-eval (cdr expr) env)]))

;; cond

(define (cond-clause? t)
  (and (list? t)
       (= (length t) 2)))

(define (cond-clause-cond c)
  (first c))

(define (cond-clause-expr c)
  (second c))

(define (cond-claue-cons b e)
  (list b e))

(define (cond? t)
  (and (tagged-list? 'cond t)
       (andmap cond-clause? (cdr t))))

(define (cond-clauses e)
  (cdr e))

(define (cond-cons cs)
  (cons 'cond cs))


;; null

(define (my-null? t)
  (eq? t 'null))

(define (null?? t)
  (tagged-tuple? 'null? 2 t))

(define (null?-expr e)
  (second e))

(define (null?-cons e)
  (list 'null? e))

;; lists
(define (expr-list-cons xs)
  (list 'list (expr-list->cons xs)))

(define (expr-list->cons xs)
  (if (null? xs)
      'null
      (cons-cons (car xs) (expr-list->cons (cdr xs)))))

(define (expr-list? xs)
  (and (list? xs)
       (> (length xs) 0)
       (eq? 'list (car xs))))

(define (expr-list-args xs)
  (cdr xs))

;; lambdas

(define (lambda? t)
  (and (tagged-tuple? 'lambda 3 t)
       (or (symbol? (cadr t))
           (and (list? (cadr t))
                (andmap symbol? (cadr t))))))
(define (lambda-cons vars e)
  (list 'lambda vars e))

(define (lambda-vars e)
  (cadr e))

(define (lambda-expr e)
  (caddr e))

;; lambda-rec

(define (lambda-rec? t)
  (and (tagged-tuple? 'lambda-rec 3 t)
       (list? (cadr t))
       (>= (length (cadr t)) 1)
       (andmap symbol? (cadr t))))

(define (lambda-rec-cons vars e)
  (list 'lambda-rec vars e))

(define (lambda-rec-expr e)
  (third e))

(define (lambda-rec-name e)
  (car (second e)))

(define (lambda-rec-vars e)
  (cdr (second e)))

;; applications

(define (app? t)
  (and (list? t)
       (> (length t) 0)))

(define (app-cons proc args)
  (cons proc args))

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
      (and (cons? t)
           (expr? (cons-fst t))
           (expr? (cons-snd t)))
      (and (car? t)
           (expr? (car-expr t)))
      (and (cdr? t)
           (expr? (cdr-expr t)))
      (and (pair?? t)
           (expr? (pair?-expr t)))
      (and (null?? t)
           (expr? (null?-expr t)))
      (and (if? t)
           (expr? (if-cond t))
           (expr? (if-then t))
           (expr? (if-else t)))
      (and (cond? t)
           (andmap (lambda (c)
                      (and (expr? (cond-clause-cond c))
                           (expr? (cond-clause-expr c))))
                   (cond-clauses t)))
      (and (lambda? t)
           (expr? (lambda-expr t)))
      (var? t)
      (and (app? t)
           (expr? (app-proc t))
           (andmap expr? (app-args t)))))

;; environments

(define empty-env
  null)

(define (add-to-env tag x v env)
  (cons (list tag x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (cadar env))
         (if (eq? (caar env) 'lazy-let)
             ((caddar env))
             (caddar env))]
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

;; closure-rec

(define (closure-rec? t)
  (tagged-tuple? 'closure-rec 5 t))

(define (closure-rec-name e)
  (second e))

(define (closure-rec-vars e)
  (third e))

(define (closure-rec-expr e)
  (fourth e))

(define (closure-rec-env e)
  (fifth e))

(define (closure-rec-cons f xs e env)
  (list 'closure-rec f xs e env))

;; evaluator

(define (bool->val b)
  (if b 'true 'false))

(define (val->bool s)
  (cond [(eq? s 'true)  true]
        [(eq? s 'false) false]
        [else (error "could not convert symbol to bool")]))

(define (eval-env e env)
  (cond [(const? e)
         e]
        [(op? e)
         (apply (op->proc (op-op e))
                (map (lambda (a) (eval-env a env))
                     (op-args e)))]
        [(let? e)
         (eval-env (let-expr e)
                   (env-for-let 'not-lazy-let (let-def e) env))]
        [(lazy-let? e)
         (eval-env (let-expr e)
                   (env-for-let 'lazy-let (let-def e) env))]
        [(my-null? e)
         null]
        [(cons? e)
         (cons (eval-env (cons-fst e) env)
               (eval-env (cons-snd e) env))]
        [(car? e)
         (car (eval-env (car-expr e) env))]
        [(cdr? e)
         (cdr (eval-env (cdr-expr e) env))]
        [(pair?? e)
         (bool->val (pair? (eval-env (pair?-expr e) env)))]
        [(null?? e)
         (bool->val (null? (eval-env (null?-expr e) env)))]
        [(if? e)
         (if (val->bool (eval-env (if-cond e) env))
             (eval-env (if-then e) env)
             (eval-env (if-else e) env))]
        [(cond? e)
         (eval-cond-clauses (cond-clauses e) env)]
        [(expr-list? e) (eval-env (expr-list->cons (expr-list-args e)) env)]
        [(expr-and? e) (expr-and-eval (cdr e) env)]
        [(expr-or? e) (expr-or-eval (cdr e) env)]
        [(var? e)
         (find-in-env (var-var e) env)]
        [(lambda? e)
         (closure-cons (lambda-vars e) (lambda-expr e) env)]
        [(lambda-rec? e)
         (closure-rec-cons (lambda-rec-name e)
                           (lambda-rec-vars e)
                           (lambda-rec-expr e)
                           env)]
        [(app? e)
         (apply-closure
           (eval-env (app-proc e) env)
           (map (lambda (a) (eval-env a env))
                (app-args e)))]))

(define (eval-cond-clauses cs env)
  (if (null? cs)
      (error "no true clause in cond")
      (let ([cond (cond-clause-cond (car cs))]
            [expr (cond-clause-expr (car cs))])
           (if (val->bool (eval-env cond env))
               (eval-env expr env)
               (eval-cond-clauses (cdr cs) env)))))

(define (apply-closure c args)
  (cond [(closure? c)
         (eval-env
            (closure-expr c)
            (env-for-closure
              (closure-vars c)
              args
              (closure-env c)))]
        [(closure-rec? c)
         (eval-env
           (closure-rec-expr c)
           (add-to-env
            'doesnt-matter-anyway
            (closure-rec-name c)
            c
            (env-for-closure
              (closure-rec-vars c)
              args
              (closure-rec-env c))))]))

(define (env-for-closure xs vs env)
  (cond [(symbol? xs) (add-to-env 'doesnt-matter-anyway xs vs env)]
        [(and (null? xs) (null? vs)) env]
        [(and (not (null? xs)) (not (null? vs)))
         (add-to-env
          'doesnt-matter-anyway
           (car xs)
           (car vs)
           (env-for-closure (cdr xs) (cdr vs) env))]
        [else (error "arity mismatch")]))

(define (env-for-let tag def env)
  (add-to-env
    tag
    (let-def-var def)
    (cond [(eq? tag 'not-lazy-let)
           (eval-env (let-def-expr def) env)]
          [(eq? tag 'lazy-let) (lambda () (eval-env (let-def-expr def) env))])
    env))

(define (eval e)
  (eval-env e empty-env))


;; ----- ZMIANY -----
;; Rozszerzylem srodowisko o tag.
;; Procedura add-to-env ma dodatkowy argument -- tag
;; Gdy env-for-let dostaje tag 'lazy-let, to dodaje do srodowiska
;;  ewaluacje wyrazenia z definicji leta zamknieta w lambdzie.
;;  W przeciwnym przypadku procedura dziala tak samo jak przed zmianami.
;; Procedura find-in-env rowniez zostala rozszerzona o tag.
;;  Jesli znajdziemy zmienna w srodowisku, to spradzamy jej tag.
;;  Jesli jest to 'lazy-let to zwracamy wywolana wartosc (bezargumentowana lambde)
;;  tej zmiennej. W p.p po prostu zwracamy jej wartosc.

(display "5! = ")
(eval '((lambda-rec (fact n)
                     (lazy-let [t 1]
                               (lazy-let [f (* n (fact (- n 1)))]
                                         (if (= n 0) t f)))) 5)) ;; powinno dac 120

(display "0! = ")
(eval '((lambda-rec (fact n)
                     (lazy-let [t 1]
                               (lazy-let [f (* n (fact (- n 1)))]
                                         (if (= n 0) t f)))) 0))

(display "suma liczb calkowitych od 1 do 10 = ")
(eval '((lambda-rec (sum n)
                    (lazy-let [t 0]
                              (lazy-let [f (+ n (sum (- n 1)))]
                                        (if (= n 0) t f)))) 10)) ;; powinno dac 55
(display "hanoi(4) = ")
(eval '((lambda-rec (hanoi n)
                    (lazy-let [t 1]
                              (lazy-let [f (+ 1
                                              (* 2 (hanoi (- n 1))))]
                                        (if (= n 1) t f)))) 4)) ;; powinno dac 15

(display "fib(6) = ")
(eval '((lambda-rec (fib n)
                    (lazy-let [f (+ (fib (- n 1)) (fib (- n 2)))]
                              (cond
                                [(= 0 n) 0]
                                [(= 1 n) 1]
                                [true f]))) 6)) ;; powinno dac 8

(display "(let (x 5) (lazy-let (y x) (let (x 10) y))) = ")
(eval '(let (x 5) (lazy-let (y x) (let (x 10) y)))) ;; powinno dac 5

(display "(lazy-let [x (/ 5 0)] 7)) = ")
(eval '(lazy-let [x (/ 5 0)] 7)) ;; powinno dac 7

(display "(lazy-let [x (lazy-let (y (/ 5 0)) (lazy-let (y 10) y))] x)) = ")
(eval '(lazy-let [x (lazy-let (y (/ 5 0)) (lazy-let (y 10) y))] x)) ;; powinno dac 10
