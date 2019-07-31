#lang racket
;; Lista 14 zadania 2, 3, 4
(define (make-cycle xs front)
  (define (helper ys)
    (if (null? (mcdr ys))
        (set-mcdr! ys front)
        (helper (mcdr ys))))
  (if (null? xs)
      (error "List must be not empty")
      (helper xs)))
      

(define xs (mcons 1 (mcons 2 (mcons 3 null))))
(make-cycle xs xs)

(define (has-cycle? xs)
  (define (walk p1 p2)
    (cond [(null? p2) #f]
          [(eq? p1 p2) #t]
          [else
           (if (null? (mcdr p2))
               #f
               (walk (mcdr p1) (mcdr (mcdr p2))))]))
  (if (null? xs)
      #f
      (walk xs (mcdr xs))))


(define (make-monitored proc)
  (let ((counter 0))
    (define (use-proc . xs)
      (set! counter (+ counter 1))
      (apply proc xs))
    (define (dispatch m)
      (cond [(eq? m 'how-many) counter]
            [(eq? m 'reset) (set! counter 0)]
            [else (error "ayyy")]))
    (cons use-proc dispatch)))


(define (bucket-sort xs)
  (let* ((max (apply max (map (lambda (e) (car e)) xs)))
         (v (make-vector (+ 1 max) null)))
    (define (the-walking-pair ys)
      (when (not (null? ys))
        (begin (vector-set! v (caar ys) (cons (cdar ys) (vector-ref v (caar ys))))
             (the-walking-pair (cdr ys)))))
    (define (procedure2 id)
      (if (> id max)
          null
          (let ((l (reverse (vector-ref v id))))
            (append (map (lambda (e) (cons id e)) l) (procedure2 (+ id 1))))))
    (the-walking-pair xs) ;; przenosi elementy z listy do wektora
    (procedure2 0)))


#|(define fact
  (lcons 1
         (lambda () (lmap *
                          (integers-starting-from 1)
                          (fact)))))|#

(define (mreverse xs)
  (define (walk-and-replace ys times replacement)
    (if (= times 0)
        (let ([y (mcar ys)])
               (set-mcar! ys replacement)
               y)
        (walk-and-replace (mcdr ys) (- times 1) replacement)))
  (define (iter n ys)
    (if (< n 0)
        (void)
        (begin (set-mcar! ys (walk-and-replace ys n (mcar ys)))
               (iter (- n 2) (mcdr ys)))))
  (define (length ys)
    (if (null? ys)
        0
        (+ 1 (length (mcdr ys)))))
  (iter (- (length xs) 1) xs))

(define (mmap f xs)
  (if (null? xs)
      (void)
      (begin (set-mcar! xs (f (mcar xs))) (mmap f (mcdr xs)))))

(define (make-cycle-v2 xs)
  (define (walk ys)
    (if (null? (mcdr ys))
        (set-mcdr! ys xs)
        (walk (mcdr ys))))
  (walk xs))
  