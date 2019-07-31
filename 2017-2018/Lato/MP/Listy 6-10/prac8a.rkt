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
       ;(list? (cadr t))
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
                   (env-for-let (let-def e) env))]
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
            (closure-rec-name c)
            c
            (env-for-closure
              (closure-rec-vars c)
              args
              (closure-rec-env c))))]))

(define (env-for-closure xs vs env)
  (cond [(symbol? xs) (add-to-env xs vs env)]
        [(and (null? xs) (null? vs)) env]
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


(display "(- 1 1) = ")
(eval '(let (binop-op (lambda (expr) (car expr)))
         (let (binop-left (lambda (expr) (car (cdr expr))))
           (let (binop-right (lambda (expr) (car (cdr (cdr expr)))))
             (let (binop? (lambda (expr) (and (pair? expr)
                                              (or (eq? (binop-op expr) '+)
                                                  (eq? (binop-op expr) '-)
                                                  (eq? (binop-op expr) '*)
                                                  (eq? (binop-op expr) '/)))))
               (let (weird-eval (lambda-rec (weird-eval expr)
                                            (if (binop? expr) 
                                                (cond [(eq? (binop-op expr) '+) (+ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '-) (- (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '*) (* (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '/) (/ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))])
                                                expr)))
                 (weird-eval (cons '- (cons 1 (cons 1 null)))))))))) ;; powinno wyjsc 0



(display "(- (+ 2 4) (+ 3 3)) = ")
(eval '(let (binop-op (lambda (expr) (car expr)))
         (let (binop-left (lambda (expr) (car (cdr expr))))
           (let (binop-right (lambda (expr) (car (cdr (cdr expr)))))
             (let (binop? (lambda (expr) (and (pair? expr)
                                              (or (eq? (binop-op expr) '+)
                                                  (eq? (binop-op expr) '-)
                                                  (eq? (binop-op expr) '*)
                                                  (eq? (binop-op expr) '/)))))
               (let (weird-eval (lambda-rec (weird-eval expr)
                                            (if (binop? expr) 
                                                (cond [(eq? (binop-op expr) '+) (+ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '-) (- (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '*) (* (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '/) (/ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))])
                                                expr)))
                 (weird-eval (cons '-
                                   (cons
                                    (cons '+
                                          (cons 2
                                                (cons 4 null)))
                                    (cons  (cons '+
                                                 (cons 3 (cons 3 null)))
                                           null)))))))))) ;; powinno wyjsc 0

(display "(- 1 (/ 2 4)) = ")
(eval '(let (binop-op (lambda (expr) (car expr)))
         (let (binop-left (lambda (expr) (car (cdr expr))))
           (let (binop-right (lambda (expr) (car (cdr (cdr expr)))))
             (let (binop? (lambda (expr) (and (pair? expr)
                                              (or (eq? (binop-op expr) '+)
                                                  (eq? (binop-op expr) '-)
                                                  (eq? (binop-op expr) '*)
                                                  (eq? (binop-op expr) '/)))))
               (let (weird-eval (lambda-rec (weird-eval expr)
                                            (if (binop? expr) 
                                                (cond [(eq? (binop-op expr) '+) (+ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '-) (- (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '*) (* (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '/) (/ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))])
                                                expr)))
                 (weird-eval (cons '-
                                   (cons 1
                                         (cons (cons '/
                                                     (cons 2
                                                           (cons 4 null)))
                                               null)))))))))) ;;powinno wyjsc 1/2

(display "(* (- 1 (/ 2 4)) (+ 7 (- 2 7)) = ")
(eval '(let (binop-op (lambda (expr) (car expr)))
         (let (binop-left (lambda (expr) (car (cdr expr))))
           (let (binop-right (lambda (expr) (car (cdr (cdr expr)))))
             (let (binop? (lambda (expr) (and (pair? expr)
                                              (or (eq? (binop-op expr) '+)
                                                  (eq? (binop-op expr) '-)
                                                  (eq? (binop-op expr) '*)
                                                  (eq? (binop-op expr) '/)))))
               (let (weird-eval (lambda-rec (weird-eval expr)
                                            (if (binop? expr) 
                                                (cond [(eq? (binop-op expr) '+) (+ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '-) (- (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '*) (* (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '/) (/ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))])
                                                expr)))
                 (weird-eval (cons '*
                                   (cons 
                                    (cons '-
                                          (cons 1
                                                (cons (cons '/
                                                            (cons 2
                                                                  (cons 4 null)))
                                                      null)))
                                    (cons (cons '+
                                                (cons 7
                                                      (cons (cons '-
                                                                  (cons 2
                                                                        (cons 7 null)))
                                                            null)))
                                          null)))))))))) ;; powinno wyjsc 1


(display "(+ 14 (* 4 7)) = ")
(eval '(let (binop-op (lambda (expr) (car expr)))
         (let (binop-left (lambda (expr) (car (cdr expr))))
           (let (binop-right (lambda (expr) (car (cdr (cdr expr)))))
             (let (binop? (lambda (expr) (and (pair? expr)
                                              (or (eq? (binop-op expr) '+)
                                                  (eq? (binop-op expr) '-)
                                                  (eq? (binop-op expr) '*)
                                                  (eq? (binop-op expr) '/)))))
               (let (weird-eval (lambda-rec (weird-eval expr)
                                            (if (binop? expr) 
                                                (cond [(eq? (binop-op expr) '+) (+ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '-) (- (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '*) (* (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))]
                                                      [(eq? (binop-op expr) '/) (/ (weird-eval (binop-left expr)) (weird-eval (binop-right expr)))])
                                                expr)))
                 (weird-eval (cons '+
                                   (cons 14
                                         (cons (cons '*
                                                     (cons 4
                                                           (cons 7 null)))
                                               null)))))))))) ;;powinno wyjsc 42
 
