#lang racket
(define (concatMap f xs)
  (if (null? xs)
      null
      (append (f (car xs)) (concatMap f (cdr xs)))))

(define (from-to s e)
  (if (= s e)
      (list s)
      (cons s (from-to (+ s 1) e))))

(define (queens board-size)
  ;; Return the representation of a board with 0 queens inserted
  (define (empty-board)
    null)
  ;; Return the representation of a board with a new queen at
  ;; (row, col) added to the partial representation `rest'
  (define (adjoin-position row col rest)
    (cons row rest))
  ;; Return true if the queen in k-th column does not attack any of
  ;; the others
  (define (safe? positions)
    (let ((first (car positions)))
      (define (iter up down rest) ;; x is the row where queen is placed
        (if (null? rest)
            #t ; czyli ze juz sprawdzilismy cala tablice i nie znalezlismy konfliktujacych hetmanow
            (let ([check (car rest)])
              (cond
                [(= check first) #f] ; jest w tym samym wierszu
                [(= check up)    #f] ; jest w gornej przekatnej
                [(= check down)  #f] ; jest w dolnej przekatnej
                [else (iter (+ 1 up) (- down 1) (cdr rest))]))))
      (iter (+ 1 first) (- 1 first) (cdr positions))))
  ;; Return a list of all possible solutions for k first columns
  (define (queen-cols k)
    (if (= k 0)
        (list (empty-board))
        (filter
         (lambda (positions) (safe? positions))
         ;;(concatMap
           ;;(lambda (new-row)
            ;;  (map (lambda (rest-of-queens)
             ;;      (adjoin-position new-row k rest-of-queens))
            ;;  (queen-cols (- k 1))))
          ;; (from-to 1 board-size)))))

         (concatMap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (from-to 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


;(queens 4)