#lang racket

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

(define (hole? t)
  (eq? t 'hole))

(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (hole-context e) 
  (define (contains? xs x)
    (cond
      [(null? xs) #f]
      [(eq? x (car xs)) #t]
      [else (contains? (cdr xs) x)]))
  (define (add-to-set x xs)
    (if (contains? xs x)
        xs
        (cons x xs)))
  (define (iter expr acc)
    (cond
      [(hole? expr) acc]
      [(binop? expr)
       ;; przejdz do dziecka zawierajacego hole
       (if (= (num-of-holes (binop-left expr)) 1)
           (iter (binop-left expr) acc)
           (iter (binop-right expr) acc))]
      [(let? expr)
       ;; jesli w definicji leta jest hole, to zakoncz procedure
       ;; w p.p iteruj dalej
       (if (hole? (let-def-expr (let-def expr)))
           acc
           (iter (let-expr expr) (iter (let-def-var (let-def-expr expr)) acc)))]
      [(var? expr) (add-to-set expr acc)]
      [else (error "Invalid input")]))
  (if (arith/let/hole-expr? e)
      (iter e null)
      #f))

(define (test)
  (define (equal-list? xs ys)
    ;; prodecura pomocnicza sprawdzajaca czy dwie listy zawieraja te same elementy
    (define (contains? y)
      ;; prodecura pomocnicza sprawdzajaca czy element y nalezy do ustalonej listy xs
      (member y xs))
    (and (list? xs)
         (list? ys)
         (= (length xs) (length ys))
         (andmap contains? ys)))
  ;; testy
  (and (equal-list? '()    (hole-context '(+ 3 hole)))
       (equal-list? '()    (hole-context '(+ hole 3)))
       ;(equal-list? '(w/e) (hole-context '(+ 1 2))) ; (hole-context ...) zwroci falsz, bo (+ 1 2) nie jest hole-expr
       ;(equal-list? '()    (hole-context '())) ; to samo
       (equal-list? '() (hole-context '(+ (let (x 3) x) hole)))
       (equal-list? '(x y) (hole-context '(let (x 3) (let (y 7) (+ x hole)))))
       (equal-list? '(y x) (hole-context '(let (x 3) (let (y 7) (+ x hole)))))
       (equal-list? '(x)   (hole-context '(let (x 3) (let (y hole) (+ x 3)))))
       (equal-list? '()    (hole-context '(let (x hole) (let (y 7) (+ x 3)))))
       (equal-list? '(chomik kotek piesek) (hole-context '(let (piesek  1)
                                                            (let (kotek  7)
                                                              (let (piesek  9)
                                                                (let (chomik  5)
                                                                  hole))))))
       (equal-list? '(y z) (hole-context '(+ (let (x 1) (- 2 x))
                                             (let (y 3) (let (z 4) (/ hole 5))))))
       (equal-list? '() (hole-context '(+ (let (x 4) 5) hole)))))

(test)