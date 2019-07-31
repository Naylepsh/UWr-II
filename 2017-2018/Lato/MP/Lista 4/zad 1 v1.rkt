#lang racket
;; Lista 4

;; notki: append tworzy smieci

;; cw. 1
; (taka reprezentacja:
; (x1, x2, ..., xk)
; gdzie xn to numer wiersza w ktorym znajduje sie hetman
(define (empty-board) null)

(define (adjoin-position row col rest)
  ;; dolaczamy nowa pare (row . col) na koniec starej listy
  (append rest (list (cons row col))))

; sprawdzanie czy pozycja jest bezpieczna
; tj czy hetmany nie atakuja sie nawzajem
(define (safe? k position)
  ; position to lista pozycji
  ; k sie juz na niej znajduje wiec trzeba je wyodrebnic
  (let ([nth-position (list-ref (- k 1))] ; -1 bo k numerujemy od 1
        [rest (filter (lambda (g) (not (= g k)))
                      position)]
        ; sprawdzanie czy sie atakuja
        ; dlaczego wystarczy tylko abs - tj czemu nie trzeba sprawdzac + i -?
        (define (attack? q1 q2)
          (or (= (car q1) (car q2))
              (= (abs (- (car q1))) (car q2)))))) ;; TO JEST ZLE - popraw
  (define (iter q rest)
    (if (null? rest)
        #t
        (and (not (attack? q (car rest)))
             (iter q (cdr rest)))))
  (iter nth-position rest))
    

        
  
      