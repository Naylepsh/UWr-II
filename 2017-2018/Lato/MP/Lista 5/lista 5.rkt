#lang racket

#|
(define (free-vars t)
  (define (walk t fv)
    (cond
      [(var? t) t]
      [(neg? t) (walk (neg-subf t) fv)]
      [(disj? t) (insert (walk (disj-left t)
                               (insert (walk (disj-right t) fv) fv)|#

(define (var? t)
   (symbol? t))

(define (neg? t)
   (and (list? t)
         (= 2 (length t))
         (eq? 'neg (car t))))

(define (conj? t)
   (and (list? t)
         (= 3 (length t))
         (eq? 'conj (car t))))

(define (disj? t)
   (and (list? t)
         (= 3 (length t))
         (eq? 'disj (car t))))

(define (prop? f)
   (or (var? f)
       (and (neg? f)
             (prop? (neg-subf f)))
       (and (disj? f)
             (prop? (disj-left f))
             (prop? (disj-rght f)))
       (and (conj? f)
             (prop? (conj-left f))
             (prop? (conj-rght f)))))

(define (neg t)
  (list 'neg t))

(define (neg-subf t)
  (cadr t))

(define (disj t1 t2)
  (list 'disj t1 t2))

(define (disj-left t)
  (second t))

(define (disj-rght t)
  (third t))

(define (conj t1 t2)
  (list 'conj t1 t2))

(define (conj-left t)
  (second t))

(define (conj-rght t)
  (third t))

(define (literal p)
  (if (or (var? p)
          (and (neg? p)
               (var? (neg-subf p))))
      (list 'literal p)
      (error "not literal")))

(define (literal? p)
  (and (list? p)
       (= (length p) 2)
       (eq? (car p) 'literal)
       (or (var? (second p))
           (and (neg? (second p))
                (var? (neg-subf (second p)))))))

#|
(require racket/set)
(define (fv t)
  (cond
    [(var? t) (set t)]
    [(neg? t) (fv (neg-subf t))]
    [(conj? t) (set-union (free-vars (conj-left t))
                       (free-vars (conj-rght t)))]
    [(disj? t) (set-union (free-vars (disj-left t))
                       (free-vars (disj-rght t)))]
    [else (error "Unknown type")]))|#

(define (contains? xs x)
  (if (null? xs)
      #f
      (or (equal? x (car xs))
          (contains? (cdr xs) x))))

(define (free-vars form)
  (define (iter f xs)
    (cond [(var? f) (if (contains? xs f)
                        xs
                        (cons f xs))]
          [(literal? f) (iter (second f) xs)]
          [(neg? f) (iter (neg-subf f) xs)]
          [(conj? f) (iter (conj-left f) (iter (conj-rght f) xs))]
          [(disj? f) (iter (disj-left f) (iter (disj-rght f) xs))]
          [else (error "Podano błedną formułę")]))
  (iter form null))



;;;;;;;;;;;;;;;; Zad 3;;;;;;;;;;;;;;;
(define (gen-vals xs)
  (if (null? xs)
      (list null )
      (let*
          ((vss (gen-vals (cdr xs)))
           (x (car xs))
           (vst (map (lambda (vs) (cons (list x true ) vs)) vss ))
           (vsf (map (lambda (vs) (cons (list x false ) vs)) vss )))
        (append vst vsf))))

(define (get-eval v evals)
  (cond
    [(null? evals) (error "Eval not found")]
    [(eq? v (caar evals)) (cadar evals)]
    [else (get-eval v (cdr evals))]))
    

(define (eval-formula f eval)
  (cond
    [(var? f) (get-eval f eval)]
    [(literal? f) (eval-formula (cdr f) eval)]
    [(neg? f) (not (eval-formula (neg-subf f) eval))]
    [(conj? f) (and (eval-formula (conj-left f) eval)
                    (eval-formula (conj-rght f) eval))]
    [(disj? f) (or (eval-formula (disj-left f) eval)
                   (eval-formula (disj-rght f) eval))]))


(define (falsefiable-eval? f)
  (define (pseudo-iter evals)
    (cond
      [(null? evals) #f]
      [(eval-formula f (car evals)) (pseudo-iter (cdr evals))]
      [else (car evals)]))
  (pseudo-iter (gen-vals (free-vars f))))

;(define f1 (conj 'f1 (neg 'f1))) ;; same as t1 and -t1
;(define f2 (disj 't1 (neg 't1))) ;; same as t1 or -t1

;(falsefiable-eval? f1) ;; should be falsefiable for any t1
;(falsefiable-eval? f2) ;; should return f


;;;;;;;;;;;;;;;; Zad 4;;;;;;;;;;;;;;;
(define (nnf? f)
  ;; jesli jest negacja przed 'nawiasem' a zawartosc nawiasu nie jest zmienna to forma nie jest w nnf
  ;; w pozostalych przypadkach sprawdz czy forma poziom nizej jest w nnf
  (cond
    [(literal? f) #t]
    [(conj? f) (and (nnf? (conj-left f))
                    (nnf? (conj-rght f)))]
    [(disj? f) (and (nnf? (disj-left f))
                    (nnf? (disj-rght f)))]
    [(neg? f) #f]
    [else (error "Unknown type")]))

;; testy
;(nnf? (conj 'f1 (neg 'f1))) ;powinno dac #t
;(nnf? (neg (conj 'f1 (neg 'f1)))) ;powinno dac #f


;;;;;;;;;;; zad 5 ;;;;;;;;;;;;;;;
(define (convert-to-nnf-neg f)
  (cond [(var? f) (literal (neg f))]
        [(disj? f) (conj (convert-to-nnf (disj-left f))
                         (convert-to-nnf (disj-rght f)))]
        [(conj? f) (disj (convert-to-nnf (conj-left f))
                         (convert-to-nnf (conj-rght f)))]
        [(neg? f) (convert-to-nnf (neg-subf f))]
        [else (error "Błędna formuła")]))

(define (convert-to-nnf f)
  (cond [(var? f) (literal f)]
        [(disj? f) (disj (convert-to-nnf (disj-left f))
                         (convert-to-nnf (disj-rght f)))]
        [(conj? f) (conj (convert-to-nnf (conj-left f))
                         (convert-to-nnf (conj-rght f)))]
        [(neg? f) (convert-to-nnf-neg (neg-subf f))]
        [else (error "Błędna formuła")]))


(convert-to-nnf (neg (disj 'p 'q)))
;;;;;;;;;;;; zad 6 ;;;;;;;;;;;;;
;; klauzula jako lista literalow
;; cnf jako lista klauzul

; konstruktor klauzuli
(define (make-cl xs)
  (cons 'cl xs))

; predykat klauzuli
(define (cl? xs)
  (and (eq? (car xs) 'cl)
       (list? (cdr xs))
       (andmap literal? (cdr xs))))

;konstruktor cnf
(define (make-cnf xs)
  (cons 'cnf xs))

; predykat cnf
(define (cnf? xs)
  (and (pair? xs)
       (eq? (car xs) 'cnf)
       (list? (cdr xs))
       (andmap cl? (cdr xs))))

;; -- ale cos w tych dwoch nizej jest zle, lel
;; procedura pomocnicza
(define (join xss yss)
  (apply append (map (lambda (p) (map (lambda (q)
                                        (append (cdr p) (cdr q)))
                                      yss))
                     xss)))


(define (convert-to-cnf f)
  (cond
    [(literal? f) (make-cnf (list (make-cl (list f))))]
    [(conj? f) (append (convert-to-cnf (conj-left f)) (convert-to-cnf (conj-rght f)))]
    [(disj? f) (make-cnf (join (cdr (convert-to-cnf (disj-left f)))
                               (cdar (convert-to-cnf (disj-rght f)))))]))
