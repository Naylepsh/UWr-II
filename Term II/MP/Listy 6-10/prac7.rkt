#lang racket

;; expressions

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * /))))

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
        [(eq? op '/) /]))

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

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (arith/let-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith/let-expr? (op-args t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def-expr (let-def t))))
      (var? t)))

;; let-lifted expressions

(define (arith-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith-expr? (op-args t)))
      (var? t)))

(define (let-lifted-expr? t)
  (or (and (let? t)
           (let-lifted-expr? (let-expr t))
           (arith-expr? (let-def-expr (let-def t))))
      (arith-expr? t)))

;; generating a symbol using a counter

(define (number->symbol i)
  (string->symbol (string-append "x" (number->string i))))

;; environments (could be useful for something)

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; the let-lift procedure

(define (let-lift expr)
  ;; Procedura let-lift najpierw zmienia nazwy zmiennym.
  ;; Nastepnie wyciaga lety na zewnatrz, a na koncu tworzy z tego odpowiedniego leta
  (define (merge-into-let defs arith)
    ;; Procedura pomocnicza tworzaca leta na podstawie definicji i wyr. arytmetycznych
    (if (null? defs)
        arith
        (let-cons (list (first defs) (second defs)) (merge-into-let (cddr defs) arith))))
  
  (let* ([renamed-expr (rename expr)]
         [shifted-expr (lift-lets renamed-expr)]
         [big-let (merge-into-let (get-defs shifted-expr) (get-arith shifted-expr))])
    big-let))

(define (rename-with-counter expr env)
  (define (make-index x env)
    ;; Procedura make-index zwraca indeks na podstawie liczby zmiennych w srodowisku
    (define (iter env i)
      (if (null? env)
          i
          (iter (cdr env) (+ i 1))))
    (iter env 0))
  
  (define (get-index x env)
    ;; Procedura get-index wyciaga indeks odpowiedniej zmiennej ze srodowiska
    (if (eq? x (first (car env)))
        (second (car env))
        (get-index x (cdr env))))

  (define (is-in-env? x env)
    (cond
      [(null? env) #f]
      [(eq? x (car (car env))) #t]
      [else (is-in-env? x (cdr env))]))
  
  (cond
    [(let? expr)
     (let* ([var (let-def-var (let-def expr))]
            [index (make-index (let-def-var (let-def expr)) env)]
            [indexed-var (number->symbol index)]
            [updated-env (add-to-env var index env)]
            [def-expr-and-env (rename-with-counter (let-def-expr (let-def expr)) updated-env)]
            [expr-and-env (rename-with-counter (let-expr expr) updated-env)]
            [new-def (list indexed-var (car def-expr-and-env))])
       (cons (let-cons new-def (car expr-and-env)) (cdr expr-and-env)))]
    [(op? expr)
     ;; Uwaga: dziala tylko dla operatorow binarnych
     (let* ([left-and-env (rename-with-counter (second expr) env)]
            [right-and-env (rename-with-counter (third expr) (cdr left-and-env))])
       (cons (list (op-op expr) (car left-and-env) (car right-and-env)) (cdr right-and-env)))]
    [(var? expr)
     ;; Sprawdzamy czy zmienna jest zwiazana
     ;; Jesli tak to po prostu poprawiamy nazwe zmiennej,
     ;; w p.p zostawiamy zmienna w spokoju
     (if (is-in-env? expr env)
         (cons (number->symbol (get-index expr env)) env)
         (cons expr env))]
    [(const? expr)
     (cons expr env)]))

(define (rename expr)
  (car (rename-with-counter expr empty-env)))

(define (defs-and-arith def-list arith)
  ;; Struktura bedaca lista: (definicje wyr-arytmetyczne)
  (list def-list arith))

(define (get-defs x)
  ;; Selektor definicji
  (first x))

(define (get-arith x)
  ;; Selektor wyrazenia arytmetycznego
  (second x))

(define (lift-lets e)
  ;; Procedura lift-lets wyciaga lety na poczatek wyrazenia.
  (cond
    [(let? e)
     (let* ([lifted-let-expr (lift-lets (let-expr e))]
            [lifted-let-def-expr (lift-lets (let-def-expr (let-def e)))]
            [new-def (list (let-def-var (let-def e)) (get-arith lifted-let-def-expr))]
            [let-def-expr-defs (get-defs lifted-let-def-expr)]
            [let-expr-defs (get-defs lifted-let-expr)]
            [let-expr-arith (get-arith lifted-let-expr)])
       (defs-and-arith
         (append let-def-expr-defs
                 (append let-expr-defs
                         new-def))
         let-expr-arith))]
    [(op? e)
     (let ([e1 (lift-lets (second e))]
           [e2 (lift-lets (third e))])
       (defs-and-arith (append (get-defs e1) (get-defs e2))
         (list (op-op e) (get-arith e1) (get-arith e2))))]
    [(const? e) (defs-and-arith null e)]
    [(var? e) (defs-and-arith null e)]))
      

(let-lift '(let (x 7) (+ 10 (* (+ x 2) 2))))
(let-lifted-expr? (let-lift '(let (x 7) (+ 10 (* (+ x 2) 2)))))

(let-lift '(+ 10 (let (x 7) x)))
(let-lifted-expr? (let-lift '(+ 10 (let (x 7) x))))

(let-lift '(let (x 7) (let (y 5) (+ x y))))
(let-lifted-expr? (let-lift '(let (x 7) (let (y 5) (+ x y)))))

(let-lift '(let (x 7) (let (y (let (x 5) x)) (+ x y))))
(let-lifted-expr? (let-lift '(let (x 7) (let (y (let (x 5) x)) (+ x y)))))

(let-lift '(let (x (let (y 3) y)) 1))
(let-lifted-expr? (let-lift '(let (x (let (y 3) y)) 1)))

(let-lift '(let (y 3) y))
(let-lifted-expr? (let-lift '(let (y 3) y)))

(let-lift '(let (x (- 2 (let (z 3) z))) (+ x 2)))
(let-lifted-expr? (let-lift '(let (x (- 2 (let (z 3) z))) (+ x 2))))

(let-lift '(let (x (- 2 (let (z (let (x 3) x)) (+ 2 z)))) (+ x 7)))
(let-lifted-expr? (let-lift '(let (x (- 2 (let (z (let (x 3) x)) (+ 2 z)))) (+ x 7))))

(let-lift '(let (x (let (y 7) (+ 8 y))) (+ x x)))
(let-lifted-expr? (let-lift '(let (x (let (y 7) (+ 8 y))) (+ x x))))

(let-lift '(+ z (let (x 7) x)))
(let-lifted-expr? (let-lift '(+ z (let (x 7) x))))
