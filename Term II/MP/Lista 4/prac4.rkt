#lang racket

(define (inc n)
  (+ n 1))

;;; ordered elements
(define (make-elem pri val)
  (cons pri val))

(define (elem-priority x)
  (car x))

(define (elem-val x)
  (cdr x))

;;; leftist heaps (after Okasaki)

;; data representation
(define leaf 'leaf)

(define (leaf? h) (eq? 'leaf h))

(define (hnode? h)
  (and (list? h)
       (= 5 (length h))
       (eq? (car h) 'hnode)
       (natural? (caddr h))))

(define (make-node elem heap-a heap-b)
  ;;; XXX: fill in the implementation
  ;; Procedura tworzaca kopiec z elementu i dwoch kopcow.
  
  ;; Liczymy rangi kopca a i b.
  (let ([rank-of-a (rank heap-a)]
        [rank-of-b (rank heap-b)])
    ;; Jesli ranga b jest wieksza od a to lewe podrzewo = b, prawe poddrzewo = a.
    ;; W przeciwnym wypadku jest na odwrot.
    (if (< rank-of-a rank-of-b) 
        (list 'hnode elem (+ 1 rank-of-a) heap-b heap-a)
        (list 'hnode elem (+ 1 rank-of-b) heap-a heap-b))))

(define (node-elem h)
  (second h))

(define (node-left h)
  (fourth h))

(define (node-right h)
  (fifth h))

(define (hord? p h)
  (or (leaf? h)
      (<= p (elem-priority (node-elem h)))))

(define (heap? h)
  (or (leaf? h)
      (and (hnode? h)
           (heap? (node-left h))
           (heap? (node-right h))
           (<= (rank (node-right h))
               (rank (node-left h)))
           (= (rank h) (inc (rank (node-right h))))
           (hord? (elem-priority (node-elem h))
                  (node-left h))
           (hord? (elem-priority (node-elem h))
                  (node-right h)))))

(define (rank h)
  (if (leaf? h)
      0
      (third h)))

;; operations

(define empty-heap leaf)

(define (heap-empty? h)
  (leaf? h))

(define (heap-insert elt heap)
  (heap-merge heap (make-node elt leaf leaf)))

(define (heap-min heap)
  (node-elem heap))

(define (heap-pop heap)
  (heap-merge (node-left heap) (node-right heap)))

(define (heap-merge h1 h2)
  (cond
   [(leaf? h1) h2]
   [(leaf? h2) h1]
   ;; XXX: fill in the implementation

   ;; Znajdujemy najmniejsze elementy kopca 1 i 2.
   [else (let ([min-of-h1 (heap-min h1)]
               [min-of-h2 (heap-min h2)])
           ;; Scalowywujemy(?) prawe podrzewo kopca o mniejszym minimum z drugim kopcem,
           ;; a nastepnie tworzymy kopiec z elementu, kopca rekurencyjnego 
           ;; oraz lewego podrzewa kopca do ktorego ten element nalezy
           (if (< (elem-val min-of-h1) (elem-val min-of-h2)) 
               (make-node min-of-h1 (heap-merge (node-right h1) h2) (node-left h1))
               (make-node min-of-h2 (heap-merge (node-right h2) h1) (node-left h2))))]))


;;; heapsort. sorts a list of numbers.
(define (heapsort xs)
  (define (popAll h)
    (if (heap-empty? h)
        null
        (cons (elem-val (heap-min h)) (popAll (heap-pop h)))))
  (let ((h (foldl (lambda (x h)
                    (heap-insert (make-elem x x) h))
            empty-heap xs)))
    (popAll h)))

;;; check that a list is sorted (useful for longish lists)
(define (sorted? xs)
  (cond [(null? xs)              true]
        [(null? (cdr xs))        true]
        [(<= (car xs) (cadr xs)) (sorted? (cdr xs))]
        [else                    false]))

;;; generate a list of random numbers of a given length
(define (randlist len max)
  (define (aux len lst)
    (if (= len 0)
        lst
        (aux (- len 1) (cons (random max) lst))))
  (aux len null))


(heapsort '(45 303 386 426 317 154 42 389 162 316))
(heapsort '(32 115 247 173 472 234 0 433 328 365 403 138 287 172 416))
(heapsort '(42 286 313 313 21 20 116 143 479))
(heapsort (randlist 1000 1000))
