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

;;
;; WHILE
;;

; memory

(define empty-mem
  null)

(define (set-mem x v m)
  (cond [(null? m)
         (list (cons x v))]
        [(eq? x (caar m))
         (cons (cons x v) (cdr m))]
        [else
         (cons (car m) (set-mem x v (cdr m)))]))

(define (get-mem x m)
  (cond [(null? m) 0]
        [(eq? x (caar m)) (cdar m)]
        [else (get-mem x (cdr m))]))

; arith and bools

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / % = > >= < <=))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '=) =]
        [(eq? op '>) >]
        [(eq? op '>=) >=]
        [(eq? op '<)  <]
        [(eq? op '<=) <=]
        [(eq? op '%) modulo]))

(define (var? t)
  (symbol? t))

(define (eval-arith e m)
  (cond [(var? e) (get-mem e m)]
        [(op? e)
         (apply
          (op->proc (op-op e))
          (map (lambda (x) (eval-arith x m))
               (op-args e)))]
        [(const? e) e]))

;;

(define (assign? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (second t) ':=)))

(define (assign-var e)
  (first e))

(define (assign-expr e)
  (third e))

(define (if? t)
  (tagged-tuple? 'if 4 t))

(define (if-cond e)
  (second e))

(define (if-then e)
  (third e))

(define (if-else e)
  (fourth e))

(define (while? t)
  (tagged-tuple? 'while 3 t))

(define (while-cond t)
  (second t))

(define (while-expr t)
  (third t))

(define (block? t)
  (list? t))

(define (++? t)
  (and (list? t)
       (= 2 (length t))
       (symbol? (car t))
       (symbol? (cadr t))
       (eq? (car t) '++)))

(define (--? t)
  (and (list? t)
       (= 2 (length t))
       (symbol? (car t))
       (symbol? (cadr t))
       (eq? (car t) '--)))

(define (-crement-var t)
  (second t))

(define (for? t)
  (tagged-tuple? 'for 5 t))

(define (for-ass t)
  (second t))

(define (for-cond t)
  (third t))

(define (for-crement t)
  (fourth t))

(define (for-body t)
  (fifth t))

;;

(define (eval e m)
  (cond [(assign? e)
         (set-mem
          (assign-var e)
          (eval-arith (assign-expr e) m)
          m)]
        [(if? e)
         (if (eval-arith (if-cond e) m)
             (eval (if-then e) m)
             (eval (if-else e) m))]
        [(while? e)
         (if (eval-arith (while-cond e) m)
             (eval e (eval (while-expr e) m))
             m)]
        [(++? e) (set-mem
                  (-crement-var e)
                  (+ (eval-arith (-crement-var e) m) 1)
                  m)]
        [(--? e) (set-mem
                  (-crement-var e)
                  (- (eval-arith (-crement-var e) m) 1)
                  m)]
        [(for? e)
         (eval (list (for-ass e) (list 'while (for-cond e) (list (for-body e) (for-crement e)))) m)]
        [(block? e)
         (if (null? e)
             m
             (eval (cdr e) (eval (car e) m)))]))

(define (run e)
  (eval e empty-mem))

;;

(define fact10
  '( (i := 10)
     (r := 1)
     (while (> i 0)
       ( (r := (* i r))
         (i := (- i 1)) ))))

;(define (computeFact10)
;  (run fact10))

(define (fib n)
  `( (r := 0)
     (b := 1)
     (n := ,n)
     (while (> n 0)
            ( (n := (- n 1))
              (t := r)
              (r := b)
              (b := (+ t b)) ))))

(define (computeFib n)
  (run (fib n)))

(computeFib 6)

(define (primes n)
  `( (n := ,n)
     (sum := 0)
     (i := 2)
     (while (> n 0)
            ( (divs := 0)
              (j := 2)
              (while (<= (* j j) i)
                     ( (if (= (% i j) 0)
                           (divs := (+ divs 1))
                           () )
                       (j := (+ j 1)) ))
              (if (> divs 0)
                  ()
                  ( (sum := (+ sum i))
                    (n := (- n 1)) ))
              (i := (+ i 1)) ))))

;; bez modulo:
#|
(d := (/ i j))
(while (>= d 0)
  (d := (- d 1)))
(if (= d 0)...|#


(define (computePrimes n)
  (run (primes n)))

(computePrimes 1)
(computePrimes 2)
(computePrimes 3)
(computePrimes 4)
(computePrimes 6)
(computePrimes 10)


;(run test)

(define test
  '( (i := 1)
     (++ i)))

(define (oof)
  (run test))

(oof)

(define test2
  '( (i := 1)
     (-- i)))

(run test2)


(define fac10
  '(
    (r := 1)
    (for (( i := 10) (j := 7)) (> i 0) ((i := (- i 1)) (j := (+ j 2)))
      ( (r := (* i r))
        ))))

(run fac10)